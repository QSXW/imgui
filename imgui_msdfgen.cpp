#include "imgui_msdfgen.h"
#include "imgui_internal.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif
#include <cstring>

#include <cmsdfgen.h>

#include <memory>
#include <unordered_map>

MsdfgenHandle* (*CreateMsdfgenHandle)(void);
void (*DestroyMsdfgenHandle)(MsdfgenHandle**);
MsdfgenFont* (*MsdfgenHandleCreateMsdfgenFont)(MsdfgenHandle*, const uint8_t*, uint32_t, MSDFType);
void (*MsdfgenHandleDestroyMsdfgenFont)(MsdfgenHandle*, MsdfgenFont**);
int (*MsdfgenFontGetGlyphIndex)(MsdfgenFont*, int);
bool (*MsdfgenFontRasterizeGlyph)(MsdfgenFont*, float, int, GlyphInfo*, uint8_t**, int*, int*, int*);
const uint8_t* (*MsdfgenFontGetGlyphScratchBuffer)(void);
bool (*MsdfgenFontGetGlyphAdvanceX)(MsdfgenFont*, int, float*);
MsdfgenFontMetrics (*MsdfgenFontGetFontMetrics)(MsdfgenFont*);
int (*MsdfgenConvertFont)(const char*, const char*, MSDFType, MsdfgenGlyphRagen*);

#if defined(_WIN32)
static HMODULE g_CMsdfgenModule;
static char    g_CMsdfgenDllPath[1024] = "CMsdfgen.dll";
#else
static void*   g_CMsdfgenModule;
static char    g_CMsdfgenDllPath[1024] = "libCMsdfgen.so";
#endif

static int g_CMsdfgenAtlasRef;

#ifdef _WIN32
#define CMsdfgen_Dlsym(mod, name) GetProcAddress((HMODULE)(mod), (name))
#else
#define CMsdfgen_Dlsym(mod, name) dlsym((mod), (name))
#endif

#define LOADF(name) \
    do { \
        name = reinterpret_cast<decltype(name)>(CMsdfgen_Dlsym(mod, #name)); \
        if (!name) \
        { \
            CMsdfgen_ClearProcAddresses(); \
            return false; \
        } \
    } while (0)

static void CMsdfgen_ClearProcAddresses()
{
    CreateMsdfgenHandle = nullptr;
    DestroyMsdfgenHandle = nullptr;
    MsdfgenHandleCreateMsdfgenFont = nullptr;
    MsdfgenHandleDestroyMsdfgenFont = nullptr;
    MsdfgenFontGetGlyphIndex = nullptr;
    MsdfgenFontRasterizeGlyph = nullptr;
    MsdfgenFontGetGlyphScratchBuffer = nullptr;
    MsdfgenFontGetGlyphAdvanceX = nullptr;
    MsdfgenFontGetFontMetrics = nullptr;
    MsdfgenConvertFont = nullptr;
}

static bool CMsdfgen_ResolveAll(void* mod)
{
    LOADF(CreateMsdfgenHandle);
    LOADF(DestroyMsdfgenHandle);
    LOADF(MsdfgenHandleCreateMsdfgenFont);
    LOADF(MsdfgenHandleDestroyMsdfgenFont);
    LOADF(MsdfgenFontGetGlyphIndex);
    LOADF(MsdfgenFontRasterizeGlyph);
    LOADF(MsdfgenFontGetGlyphScratchBuffer);
    LOADF(MsdfgenFontGetGlyphAdvanceX);
    LOADF(MsdfgenFontGetFontMetrics);
    LOADF(MsdfgenConvertFont);
    return true;
}
#undef LOADF

static bool CMsdfgen_EnsureModule()
{
    if (g_CMsdfgenModule)
        return true;

#ifdef _WIN32
    ImVector<wchar_t> wbuf;
    int n = MultiByteToWideChar(CP_UTF8, 0, g_CMsdfgenDllPath, -1, nullptr, 0);
    if (n <= 0)
        return false;
    wbuf.resize(n);
    MultiByteToWideChar(CP_UTF8, 0, g_CMsdfgenDllPath, -1, wbuf.Data, n);
    g_CMsdfgenModule = LoadLibraryW(wbuf.Data);
#else
    g_CMsdfgenModule = dlopen(g_CMsdfgenDllPath, RTLD_NOW);
#endif
    if (!g_CMsdfgenModule)
        return false;
    if (!CMsdfgen_ResolveAll(g_CMsdfgenModule))
    {
#ifdef _WIN32
        FreeLibrary((HMODULE)g_CMsdfgenModule);
#else
        dlclose(g_CMsdfgenModule);
#endif
        g_CMsdfgenModule = nullptr;
        return false;
    }
    return true;
}

static void CMsdfgen_FreeModule()
{
    if (!g_CMsdfgenModule)
        return;
#ifdef _WIN32
    FreeLibrary((HMODULE)g_CMsdfgenModule);
#else
    dlclose(g_CMsdfgenModule);
#endif
    g_CMsdfgenModule = nullptr;
    CMsdfgen_ClearProcAddresses();
}

namespace ImGuiMsdfgen
{
// ---------------------------------------------------------------------------
// Loader internals: live CMsdfgen DLL vs pre-baked .dat + atlas pixels.
// EmToLayoutPx converts msdf "em" plane coords to ImGui layout pixels (blend of baked->Size and SizePixels/lineHeight).
// ---------------------------------------------------------------------------

struct ImGui_ImplMsdfgen_Data
{
    MsdfgenHandle* Handle;
    ImGui_ImplMsdfgen_Data() { memset((void*)this, 0, sizeof(*this)); }
};

struct ImGui_ImplMsdfgen_FontSrcData
{
    MsdfgenFont* Font;
    const MsdfgenAtlasData* Atlas;
    MsdfgenFontMetrics Metrics;
    std::unique_ptr<std::unordered_map<uint32_t, const MsdfgenGlyphData*>> GlyphMap;
    const uint8_t* PixelsRGBA;
    int Width;
    int Height;
    bool YOriginBottom;

    ImGui_ImplMsdfgen_FontSrcData() { memset((void*)this, 0, sizeof(*this)); }
};

struct ImGui_ImplMsdfgen_FontSrcBakedData
{
    MsdfgenFontMetrics Metrics{};
    bool YOriginBottom = false;
    float EmToLayoutPx = 1.0f;
};

static float ImGui_ImplMsdfgen_ComputeEmToLayoutPx(const ImFontConfig* src, ImFontBaked* baked, const MsdfgenFontMetrics& metrics)
{
    // Blend (SizePixels/lineHeight) toward baked->Size. CJK line metrics differ; blend can drift EmToLayoutPx.
    // MSDF geometry is rasterized at ImMax(IMGUI_SDF_DETAIL, baked->Size) while imgui_freetype uses FT pixel metrics
    // at baked->Size — net result is often ~1.25x larger than FreeType for the same ImFontConfig. Scale down so
    // Ascent/glyph quads/advances match hinted bitmap sizing (~0.8x empirically; tune if your DLL/atlas differs).
    static const float kLineHeightBlend = 0.25f;
    static const float kEmToLayoutVsFreetypeBitmap = 0.85f;
    const double lh = metrics.lineHeight;
    if (lh <= 0.0)
        return baked->Size * kEmToLayoutVsFreetypeBitmap;
    const float lh_px = src->SizePixels / (float)lh;
    const float em = baked->Size + (lh_px - baked->Size) * kLineHeightBlend;
    return em * kEmToLayoutVsFreetypeBitmap;
}

// CMsdfgen plane bounds are either em (~0.5 wide) or pixels at raster em; heuristic by magnitude.
static bool ImGui_ImplMsdfgen_PlaneBoundsLookLikeEm(const GlyphInfo& g)
{
    const double pw = g.pr - g.pl;
    const double ph = g.pt - g.pb;
    return pw > 0.0 && ph > 0.0 && pw < 2.5 && ph < 2.5 &&
        ImAbs(g.pl) < 2.5 && ImAbs(g.pr) < 2.5 && ImAbs(g.pb) < 2.5 && ImAbs(g.pt) < 2.5;
}

static void ImGui_ImplMsdfgen_ScaleGlyphInfo_GlyphMap(GlyphInfo* info, float em_to_layout_px, const MsdfgenAtlasData* atlas)
{
    if (ImGui_ImplMsdfgen_PlaneBoundsLookLikeEm(*info))
    {
        info->pl *= em_to_layout_px;
        info->pr *= em_to_layout_px;
        info->pb *= em_to_layout_px;
        info->pt *= em_to_layout_px;
        if (info->AdvanceX > 0.0f && info->AdvanceX < 2.0f)
            info->AdvanceX *= em_to_layout_px;
    }
    else if (atlas != nullptr && atlas->size > 0.0f)
    {
        const float s = em_to_layout_px / atlas->size;
        info->pl *= s;
        info->pr *= s;
        info->pb *= s;
        info->pt *= s;
        info->AdvanceX *= s;
    }
}

static void ImGui_ImplMsdfgen_ScaleGlyphInfo_LiveRaster(GlyphInfo* info, float em_to_layout_px, float sdf_raster_em)
{
    if (ImGui_ImplMsdfgen_PlaneBoundsLookLikeEm(*info))
    {
        info->pl *= em_to_layout_px;
        info->pr *= em_to_layout_px;
        info->pb *= em_to_layout_px;
        info->pt *= em_to_layout_px;
        if (info->AdvanceX > 0.0f && info->AdvanceX < 2.0f)
            info->AdvanceX *= em_to_layout_px;
    }
    else if ((info->Width <= 0 || info->Height <= 0) && info->AdvanceX > 0.0f && info->AdvanceX < 2.0f)
    {
        info->AdvanceX *= em_to_layout_px;
    }
    else
    {
        const float to_layout = em_to_layout_px / sdf_raster_em;
        info->AdvanceX *= to_layout;
        info->pl *= to_layout;
        info->pr *= to_layout;
        info->pb *= to_layout;
        info->pt *= to_layout;
    }
}

static bool ImGui_ImplMsdfgen_LoaderInit(ImFontAtlas* atlas)
{
    IM_ASSERT(atlas->FontLoaderData == nullptr);
    if (!CMsdfgen_EnsureModule())
        return false;
    ImGui_ImplMsdfgen_Data* bd = IM_NEW(ImGui_ImplMsdfgen_Data)();
    bd->Handle = CreateMsdfgenHandle();
    if (!bd->Handle)
    {
        IM_DELETE(bd);
        return false;
    }
    g_CMsdfgenAtlasRef++;
    atlas->FontLoaderData = (void*)bd;
    return true;
}

static void ImGui_ImplMsdfgen_LoaderShutdown(ImFontAtlas* atlas)
{
    ImGui_ImplMsdfgen_Data* bd = (ImGui_ImplMsdfgen_Data*)atlas->FontLoaderData;
    IM_ASSERT(bd != nullptr);
    if (bd->Handle)
        DestroyMsdfgenHandle(&bd->Handle);
    IM_DELETE(bd);
    atlas->FontLoaderData = nullptr;
    g_CMsdfgenAtlasRef--;
    if (g_CMsdfgenAtlasRef <= 0)
    {
        g_CMsdfgenAtlasRef = 0;
        CMsdfgen_FreeModule();
    }
}

static bool ImGui_ImplMsdfgen_FontSrcInit(ImFontAtlas* atlas, ImFontConfig* src)
{
    ImGui_ImplMsdfgen_Data* bd = (ImGui_ImplMsdfgen_Data*)atlas->FontLoaderData;
    IM_ASSERT(bd && bd->Handle);

    ImGui_ImplMsdfgen_FontSrcData* bd_font = IM_NEW(ImGui_ImplMsdfgen_FontSrcData);
    IM_ASSERT(src->FontLoaderData == nullptr);
    src->FontLoaderData = bd_font;

    const float ref_size = src->DstFont->Sources[0]->SizePixels;
    if (src->MergeMode && src->SizePixels == 0.0f)
        src->SizePixels = ref_size;

    if (src->FontPixels)
    {
        bd_font->PixelsRGBA = (const uint8_t*)src->FontPixels;
        bd_font->Width = src->width;
        bd_font->Height = src->heigth;

        const uint8_t* p = (const uint8_t*)src->FontData;
        if (p == nullptr || (size_t)src->FontDataSize < sizeof(MsdfgenAtlasData) + sizeof(MsdfgenFontMetrics))
            return false;

        bd_font->Atlas = (const MsdfgenAtlasData*)p;
        p += sizeof(MsdfgenAtlasData);
        bd_font->YOriginBottom = (strcmp(bd_font->Atlas->yOrigin, "bottom") == 0);

        bd_font->Metrics = *(const MsdfgenFontMetrics*)p;
        p += sizeof(MsdfgenFontMetrics);

        const size_t remaining = (size_t)src->FontDataSize - (size_t)(p - (const uint8_t*)src->FontData);
        const size_t num_glyphs = remaining / sizeof(MsdfgenGlyphData);
        const MsdfgenGlyphData* glyph_data = (const MsdfgenGlyphData*)p;

        bd_font->GlyphMap.reset(new std::unordered_map<uint32_t, const MsdfgenGlyphData*>{});
        bd_font->GlyphMap->reserve(num_glyphs);
        for (size_t i = 0; i < num_glyphs; i++)
            bd_font->GlyphMap->insert({ glyph_data[i].codepoint, glyph_data + i });

        return true;
    }

    bd_font->Font = MsdfgenHandleCreateMsdfgenFont(bd->Handle, (const uint8_t *) src->FontData, (uint32_t) src->FontDataSize, MSDFType::MTSDF);
    if (!bd_font->Font)
        return false;

    bd_font->Metrics = MsdfgenFontGetFontMetrics(bd_font->Font);

    return true;
}

static void ImGui_ImplMsdfgen_FontSrcDestroy(ImFontAtlas* atlas, ImFontConfig* src)
{
    ImGui_ImplMsdfgen_Data* bd = (ImGui_ImplMsdfgen_Data*)atlas->FontLoaderData;
    ImGui_ImplMsdfgen_FontSrcData* bd_font = (ImGui_ImplMsdfgen_FontSrcData*)src->FontLoaderData;
    if (bd_font)
    {
        if (bd_font->Font)
            MsdfgenHandleDestroyMsdfgenFont(bd->Handle, &bd_font->Font);
        IM_DELETE(bd_font);
    }
    src->FontLoaderData = nullptr;
}

static bool ImGui_ImplMsdfgen_FontSrcContainsGlyph(ImFontAtlas* atlas, ImFontConfig* src, ImWchar codepoint)
{
    IM_UNUSED(atlas);
    ImGui_ImplMsdfgen_FontSrcData* bd_font = (ImGui_ImplMsdfgen_FontSrcData*)src->FontLoaderData;
    IM_ASSERT(bd_font != nullptr);

    if (bd_font->GlyphMap)
        return bd_font->GlyphMap->find((uint32_t)codepoint) != bd_font->GlyphMap->end();

    if (!bd_font->Font)
        return false;

    if (MsdfgenFontGetGlyphIndex(bd_font->Font, (int)codepoint) != 0)
        return true;
    // Space may use glyph index 0; confirm via advance.
    if (codepoint == (ImWchar)' ')
    {
        float adv = 0.0f;
        return MsdfgenFontGetGlyphAdvanceX(bd_font->Font, (int)codepoint, &adv);
    }
    return false;
}

static bool ImGui_ImplMsdfgen_FontBakedInit(ImFontAtlas* atlas, ImFontConfig* src, ImFontBaked* baked, void* loader_data_for_baked_src)
{
    IM_UNUSED(atlas);
    ImGui_ImplMsdfgen_FontSrcData* bd_font = (ImGui_ImplMsdfgen_FontSrcData*)src->FontLoaderData;
    ImGui_ImplMsdfgen_FontSrcBakedData* bd_baked = (ImGui_ImplMsdfgen_FontSrcBakedData*)loader_data_for_baked_src;
    IM_ASSERT(bd_font && bd_baked);

    IM_PLACEMENT_NEW(bd_baked) ImGui_ImplMsdfgen_FontSrcBakedData();

    bd_baked->Metrics = bd_font->Metrics;
    bd_baked->EmToLayoutPx = ImGui_ImplMsdfgen_ComputeEmToLayoutPx(src, baked, bd_baked->Metrics);
    if (src->MergeMode == false)
    {
        baked->Ascent = (float)(bd_baked->Metrics.ascenderY * bd_baked->EmToLayoutPx);
        baked->Descent = (float)(bd_baked->Metrics.descenderY * bd_baked->EmToLayoutPx);
    }
    bd_baked->YOriginBottom = bd_font->YOriginBottom;

    return true;
}

static void ImGui_ImplMsdfgen_FontBakedDestroy(ImFontAtlas* atlas, ImFontConfig* src, ImFontBaked* baked, void* loader_data_for_baked_src)
{
    IM_UNUSED(atlas);
    IM_UNUSED(src);
    IM_UNUSED(baked);
    ImGui_ImplMsdfgen_FontSrcBakedData* bd_baked = (ImGui_ImplMsdfgen_FontSrcBakedData*)loader_data_for_baked_src;
    if (!bd_baked)
        return;
    bd_baked->~ImGui_ImplMsdfgen_FontSrcBakedData();
}

static bool ImGui_ImplMsdfgen_FontBakedLoadGlyph(ImFontAtlas* atlas, ImFontConfig* src, ImFontBaked* baked, void* loader_data_for_baked_src, ImWchar codepoint, ImFontGlyph* out_glyph, float* out_advance_x)
{
    ImGui_ImplMsdfgen_FontSrcData* bd_font = (ImGui_ImplMsdfgen_FontSrcData*)src->FontLoaderData;
    ImGui_ImplMsdfgen_FontSrcBakedData* bd_baked = (ImGui_ImplMsdfgen_FontSrcBakedData*)loader_data_for_baked_src;
    IM_ASSERT(bd_font && bd_baked);

    // Advance-only (CalcTextSize / layout): DLL returns em-based advance; scale to layout pixels.
    if (out_advance_x != nullptr)
    {
        IM_ASSERT(out_glyph == nullptr);
        if (bd_font->GlyphMap)
        {
            auto it = bd_font->GlyphMap->find((uint32_t)codepoint);
            if (it == bd_font->GlyphMap->end())
                return false;
            const float adv = it->second->advance;
            if (bd_font->Atlas != nullptr && bd_font->Atlas->size > 0.0f)
                *out_advance_x = adv * (bd_baked->EmToLayoutPx / bd_font->Atlas->size);
            else
                *out_advance_x = adv * bd_baked->EmToLayoutPx;
            return true;
        }
        if (!bd_font->Font)
            return false;
        if (!MsdfgenFontGetGlyphAdvanceX(bd_font->Font, (int)codepoint, out_advance_x))
            return false;
        *out_advance_x *= bd_baked->EmToLayoutPx;
        return true;
    }

    GlyphInfo info{};
    const uint8_t* src_pixels = nullptr;
    int src_stride = 0;
    int live_channels = -1;

    // Pre-baked atlas + glyph table from .dat
    if (bd_font->GlyphMap)
    {
        auto it = bd_font->GlyphMap->find((uint32_t)codepoint);
        if (it == bd_font->GlyphMap->end())
            return false;
        const MsdfgenGlyphData* g = it->second;

        info.Width = (int)g->width;
        info.Height = (int)g->height;
        info.OffsetX = (int)g->x;
        info.OffsetY = (int)g->y;
        info.AdvanceX = g->advance;
        info.IsColored = false;
        info.pl = g->planeBoundsLeft;
        info.pr = g->planeBoundsRight;
        info.pb = g->planeBoundsBottom;
        info.pt = g->planeBoundsTop;
        ImGui_ImplMsdfgen_ScaleGlyphInfo_GlyphMap(&info, bd_baked->EmToLayoutPx, bd_font->Atlas);

        const int atlas_w = bd_font->Width;
        const int atlas_h = bd_font->Height;
        const int gx = (int)g->x;
        const int gy = (int)g->y;
        const int row = bd_font->YOriginBottom ? (atlas_h - gy - (int)g->height) : gy;
        src_stride = atlas_w * 4;
        src_pixels = bd_font->PixelsRGBA + (size_t)(row * atlas_w + gx) * 4;
    }
    else
    {
        if (!bd_font->Font)
            return false;
        if (MsdfgenFontGetGlyphIndex(bd_font->Font, (int)codepoint) == 0 && codepoint != (ImWchar)' ')
            return false;

        const float sdf_raster_em = ImMax((float)IMGUI_SDF_DETAIL, baked->Size);

        GlyphInfo rinfo{};
        uint8_t* out_pixels = nullptr;
        int rw = 0, rh = 0, ch = 0;
        if (!MsdfgenFontRasterizeGlyph(bd_font->Font, sdf_raster_em, (int)codepoint, &rinfo, &out_pixels, &rw, &rh, &ch))
            return false;
        info = rinfo;
        IM_ASSERT(rw == info.Width && rh == info.Height);
        ImGui_ImplMsdfgen_ScaleGlyphInfo_LiveRaster(&info, bd_baked->EmToLayoutPx, sdf_raster_em);

        if (out_pixels == nullptr || info.Width <= 0 || info.Height <= 0)
        {
            out_glyph->Codepoint = codepoint;
            out_glyph->AdvanceX = info.AdvanceX;
            return true;
        }

        if (ch == 4)
        {
            src_pixels = out_pixels;
            src_stride = info.Width * 4;
        }
        else if (ch == 1)
        {
            src_pixels = out_pixels;
            src_stride = info.Width;
        }
        else
            return false;
        live_channels = ch;
    }

    out_glyph->Codepoint = codepoint;
    out_glyph->AdvanceX = info.AdvanceX;

    const int w = info.Width;
    const int h = info.Height;
    const bool is_visible = (w > 0 && h > 0);
    if (!is_visible)
        return true;

    IM_ASSERT(w <= 256 && h <= 256);

    ImFontAtlasRectId pack_id = ImFontAtlasPackAddRect(atlas, w, h);
    if (pack_id == ImFontAtlasRectId_Invalid)
    {
        IM_ASSERT(pack_id != ImFontAtlasRectId_Invalid && "Out of texture memory.");
        return false;
    }
    ImTextureRect* r = ImFontAtlasPackGetRect(atlas, pack_id);

    // Quad in layout pixels (same structure as imgui_freetype FontBakedLoadGlyph: font_off_* + glyph_off_*).
    // Plane bounds (pl,pr,pt,pb) are in the same scaled space as the packed SDF quad; use them for X/Y — do not
    // replace extents with pixel w/h, which can disagree with plane units after ScaleGlyphInfo_* and stretch the UV quad.
    const float ref_size = baked->OwnerFont->Sources[0]->SizePixels;
    const float offsets_scale = (ref_size != 0.0f) ? (baked->Size / ref_size) : 1.0f;
    float font_off_x = ImFloor(src->GlyphOffset.x * offsets_scale + 0.5f);
    float font_off_y = ImFloor(src->GlyphOffset.y * offsets_scale + 0.5f) + baked->Ascent;
    float glyph_off_x0 = (float)info.pl;
    float glyph_off_x1 = (float)info.pr;
    float glyph_off_y0 = -(float)info.pt;
    float glyph_off_y1 = -(float)info.pb;
    out_glyph->X0 = glyph_off_x0 + font_off_x;
    out_glyph->X1 = glyph_off_x1 + font_off_x;
    out_glyph->Y0 = glyph_off_y0 + font_off_y;
    out_glyph->Y1 = glyph_off_y1 + font_off_y;
    out_glyph->Visible = true;
    out_glyph->Colored = false;
    out_glyph->PackId = pack_id;

    // Upload to atlas (pre-baked RGBA slice vs live SDF/MSDF)
    if (bd_font->GlyphMap)
    {
        ImFontAtlasBakedSetFontGlyphBitmap(atlas, baked, src, out_glyph, r, src_pixels, ImTextureFormat_RGBA32, src_stride);
        atlas->TexPixelsUseColors = true;
    }
    else if (live_channels == 1)
    {
        ImFontAtlasBakedSetFontGlyphBitmap(atlas, baked, src, out_glyph, r, src_pixels, ImTextureFormat_Alpha8, w);
        atlas->TexPixelsUseColors = true;
    }
    else
    {
        ImFontAtlasBakedSetFontGlyphBitmap(atlas, baked, src, out_glyph, r, src_pixels, ImTextureFormat_RGBA32, w * 4);
        atlas->TexPixelsUseColors = true;
    }
    return true;
}

const ImFontLoader* GetFontLoader()
{
    static ImFontLoader loader;
    loader.Name = "CMsdfgen";
    loader.LoaderInit = ImGui_ImplMsdfgen_LoaderInit;
    loader.LoaderShutdown = ImGui_ImplMsdfgen_LoaderShutdown;
    loader.FontSrcInit = ImGui_ImplMsdfgen_FontSrcInit;
    loader.FontSrcDestroy = ImGui_ImplMsdfgen_FontSrcDestroy;
    loader.FontSrcContainsGlyph = ImGui_ImplMsdfgen_FontSrcContainsGlyph;
    loader.FontBakedInit = ImGui_ImplMsdfgen_FontBakedInit;
    loader.FontBakedDestroy = ImGui_ImplMsdfgen_FontBakedDestroy;
    loader.FontBakedLoadGlyph = ImGui_ImplMsdfgen_FontBakedLoadGlyph;
    loader.FontBakedSrcLoaderDataSize = sizeof(ImGui_ImplMsdfgen_FontSrcBakedData);
    return &loader;
}

static void* (*GImGuiMsdfgenAllocFunc)(size_t size, void* user_data) = nullptr;
static void (*GImGuiMsdfgenFreeFunc)(void* ptr, void* user_data) = nullptr;
static void* GImGuiMsdfgenAllocatorUserData = nullptr;

void SetCMsdfgenDllPath(const char* path)
{
#if defined(_WIN32)
    const char* fallback = "CMsdfgen.dll";
#else
    const char* fallback = "libCMsdfgen.so";
#endif
    if (!path || !path[0])
    {
        strncpy(g_CMsdfgenDllPath, fallback, sizeof(g_CMsdfgenDllPath) - 1);
    }
    else
    {
        strncpy(g_CMsdfgenDllPath, path, sizeof(g_CMsdfgenDllPath) - 1);
    }
    g_CMsdfgenDllPath[sizeof(g_CMsdfgenDllPath) - 1] = 0;
}

void SetAllocatorFunctions(void* (*alloc_func)(size_t sz, void* user_data), void (*free_func)(void* ptr, void* user_data), void* user_data)
{
    GImGuiMsdfgenAllocFunc = alloc_func;
    GImGuiMsdfgenFreeFunc = free_func;
    GImGuiMsdfgenAllocatorUserData = user_data;
}

} // namespace ImGuiMsdfgen
