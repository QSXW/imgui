#include "imgui_msdfgen.h"
#include "imgui_internal.h"
#include <cmsdfgen.h>
#include <unordered_map>
#include <memory>

#ifndef STB_RECT_PACK_IMPLEMENTATION                        // in case the user already have an implementation in the _same_ compilation unit (e.g. unity builds)
#ifndef IMGUI_DISABLE_STB_RECT_PACK_IMPLEMENTATION
#define STBRP_ASSERT(x)     do { IM_ASSERT(x); } while (0)
#define STBRP_STATIC
#define STB_RECT_PACK_IMPLEMENTATION
#endif
#ifdef IMGUI_STB_RECT_PACK_FILENAME
#include IMGUI_STB_RECT_PACK_FILENAME
#else
#include "imstb_rectpack.h"
#endif
#endif

namespace ImGuiMsdfgen
{

struct ImFontBuildSrcDataFT
{
    MsdfgenFont                                                       *Font;
    stbrp_rect                                                        *Rects;              // Rectangle to pack. We first fill in their size and the packer will give us their position.
    const ImWchar                                                     *SrcRanges;          // Ranges as requested by user (user is allowed to request too much, e.g. 0x0020..0xFFFF)
    int                                                                DstIndex;           // Index into atlas->Fonts[] and dst_tmp_array[]
    int                                                                GlyphsHighest;      // Highest requested codepoint
    int                                                                GlyphsCount;        // Glyph count (excluding missing glyphs and glyphs already set by an earlier source font)
    ImBitVector                                                        GlyphsSet;          // Glyph bit map (random access, 1-bit per codepoint. This will be a maximum of 8KB)
    ImVector<ImFontBuildSrcGlyph>                                      GlyphsList;
    MsdfgenAtlasData                                                  *Atlas;
    MsdfgenFontMetrics                                                *Metrics;
	std::unique_ptr<std::unordered_map<uint32_t, MsdfgenGlyphData *>>  GlyphMap;
    uint8_t                                                           *pixels;
    int                                                                width;
    int                                                                height;
	bool                                                               bottom;
};

// Temporary data for one destination ImFont* (multiple source fonts can be merged into one destination ImFont)
struct ImFontBuildDstDataFT
{
    int                 SrcCount;           // Number of source fonts targeting this destination font.
    int                 GlyphsHighest;
    int                 GlyphsCount;
    ImBitVector         GlyphsSet;          // This is used to resolve collision when multiple sources are merged into a same destination font.
};

bool ImFontAtlasBuildWithMsdfgen(MsdfgenHandle *handle, ImFontAtlas* atlas, unsigned int extra_flags)
{
    IM_ASSERT(atlas->ConfigData.Size > 0);

    int numChannels = 4;
	int sourceChannels = numChannels;
    MSDFType type = MSDFType::MTSDF;

    ImFontAtlasBuildInit(atlas);

    // Clear atlas
    atlas->TexID           = (ImTextureID)NULL;
    atlas->TexWidth        = atlas->TexHeight = 0;
    atlas->TexUvScale      = ImVec2(0.0f, 0.0f);
    atlas->TexUvWhitePixel = ImVec2(0.0f, 0.0f);
    atlas->ClearTexData();

    // Temporary storage for building
    bool src_load_color = true;
    ImVector<ImFontBuildSrcDataFT> src_tmp_array;
    ImVector<ImFontBuildDstDataFT> dst_tmp_array;
    src_tmp_array.resize(atlas->ConfigData.Size);
    dst_tmp_array.resize(atlas->Fonts.Size);
    memset((void*)src_tmp_array.Data, 0, (size_t)src_tmp_array.size_in_bytes());
    memset((void*)dst_tmp_array.Data, 0, (size_t)dst_tmp_array.size_in_bytes());

    // 1. Initialize font loading structure, check font data validity
    for (int src_i = 0; src_i < atlas->ConfigData.Size; src_i++)
    {
        ImFontBuildSrcDataFT& src_tmp = src_tmp_array[src_i];
        ImFontConfig& cfg = atlas->ConfigData[src_i];
        auto &sdf_font    = src_tmp.Font;
        IM_ASSERT(cfg.DstFont && (!cfg.DstFont->IsLoaded() || cfg.DstFont->ContainerAtlas == atlas));

        // Find index from cfg.DstFont (we allow the user to set cfg.DstFont. Also it makes casual debugging nicer than when storing indices)
        src_tmp.DstIndex = -1;
        for (int output_i = 0; output_i < atlas->Fonts.Size && src_tmp.DstIndex == -1; output_i++)
            if (cfg.DstFont == atlas->Fonts[output_i])
                src_tmp.DstIndex = output_i;
        IM_ASSERT(src_tmp.DstIndex != -1); // cfg.DstFont not pointing within atlas->Fonts[] array?
        if (src_tmp.DstIndex == -1)
            return false;

        if (cfg.FontPixels)
        {
			src_tmp.pixels = (uint8_t *)cfg.FontPixels;
            src_tmp.width  = cfg.width;
			src_tmp.height = cfg.heigth;
			uint8_t *p = (uint8_t *)cfg.FontData;
			src_tmp.Atlas   = (MsdfgenAtlasData *)p;
			p += sizeof(MsdfgenAtlasData);

            sourceChannels = 4;
            if (!strcmp(src_tmp.Atlas->type, "sdf") || !strcmp(src_tmp.Atlas->type, "psdf"))
            {
                numChannels = 1;
            }
            else if (!strcmp(src_tmp.Atlas->type, "msdf"))
            {
                numChannels = 3;
			}
            else if (!strcmp(src_tmp.Atlas->type, "mtsdf"))
            {
				numChannels = 4;
            }

            src_tmp.bottom = !strcmp(src_tmp.Atlas->yOrigin, "bottom");
			numChannels = 4;
			src_tmp.Metrics = (MsdfgenFontMetrics *)p;
			p += sizeof(MsdfgenFontMetrics);

            uint32_t numGlyph = (cfg.FontDataSize - size_t(p - (uint8_t *)cfg.FontData)) / sizeof(MsdfgenGlyphData);
			MsdfgenGlyphData *glyphData = (MsdfgenGlyphData *)p;

            src_tmp.GlyphMap.reset(new std::unordered_map<uint32_t, MsdfgenGlyphData *>{});
            for (size_t i = 0; i < numGlyph; i++)
            {
				src_tmp.GlyphMap->insert({glyphData[i].codepoint, glyphData + i});
            }
        }
        else
        {
			src_tmp.bottom = false;
			sdf_font = MsdfgenHandleCreateMsdfgenFont(handle, (const uint8_t *) cfg.FontData, cfg.FontDataSize, type);
			if (!sdf_font)
			{
				return false;
			}
        }

        // Measure highest codepoints
        src_load_color |= (cfg.FontBuilderFlags & ImGuiMsdfgenFlags_LoadColor) != 0;
        ImFontBuildDstDataFT& dst_tmp = dst_tmp_array[src_tmp.DstIndex];
        src_tmp.SrcRanges = cfg.GlyphRanges ? cfg.GlyphRanges : atlas->GetGlyphRangesDefault();
        for (const ImWchar* src_range = src_tmp.SrcRanges; src_range[0] && src_range[1]; src_range += 2)
            src_tmp.GlyphsHighest = ImMax(src_tmp.GlyphsHighest, (int)src_range[1]);
        dst_tmp.SrcCount++;
        dst_tmp.GlyphsHighest = ImMax(dst_tmp.GlyphsHighest, src_tmp.GlyphsHighest);
    }

    // 2. For every requested codepoint, check for their presence in the font data, and handle redundancy or overlaps between source fonts to avoid unused glyphs.
    int total_glyphs_count = 0;
    for (int src_i = 0; src_i < src_tmp_array.Size; src_i++)
    {
        ImFontConfig &cfg = atlas->ConfigData[src_i];
        ImFontBuildSrcDataFT& src_tmp = src_tmp_array[src_i];
        ImFontBuildDstDataFT& dst_tmp = dst_tmp_array[src_tmp.DstIndex];
        src_tmp.GlyphsSet.Create(src_tmp.GlyphsHighest + 1);
        if (dst_tmp.GlyphsSet.Storage.empty())
            dst_tmp.GlyphsSet.Create(dst_tmp.GlyphsHighest + 1);

        if (cfg.FontPixels)
		{
		    for (const ImWchar *src_range = src_tmp.SrcRanges; src_range[0] && src_range[1]; src_range += 2)
		    {
			    for (int codepoint = src_range[0]; codepoint <= (int) src_range[1]; codepoint++)
			    {
				    if (dst_tmp.GlyphsSet.TestBit(codepoint))        // Don't overwrite existing glyphs. We could make this an option (e.g. MergeOverwrite)
					    continue;

                    auto it = src_tmp.GlyphMap->find(codepoint);
                    if (it == src_tmp.GlyphMap->end())
                    {
						continue;
                    }

				    // Add to avail set/counters
				    src_tmp.GlyphsCount++;
				    dst_tmp.GlyphsCount++;
				    src_tmp.GlyphsSet.SetBit(codepoint);
				    dst_tmp.GlyphsSet.SetBit(codepoint);
				    total_glyphs_count++;
			    }
		    }
		}
		else
		{
			for (const ImWchar *src_range = src_tmp.SrcRanges; src_range[0] && src_range[1]; src_range += 2)
			{
				for (int codepoint = src_range[0]; codepoint <= (int) src_range[1]; codepoint++)
				{
					if (dst_tmp.GlyphsSet.TestBit(codepoint))        // Don't overwrite existing glyphs. We could make this an option (e.g. MergeOverwrite)
						continue;

					uint32_t glyph_index = MsdfgenFontGetGlyphIndex(src_tmp.Font, codepoint);
					if (glyph_index == 0)
						continue;

					// Add to avail set/counters
					src_tmp.GlyphsCount++;
					dst_tmp.GlyphsCount++;
					src_tmp.GlyphsSet.SetBit(codepoint);
					dst_tmp.GlyphsSet.SetBit(codepoint);
					total_glyphs_count++;
				}
			}
		}
    }

    // 3. Unpack our bit map into a flat list (we now have all the Unicode points that we know are requested _and_ available _and_ not overlapping another)
    for (int src_i = 0; src_i < src_tmp_array.Size; src_i++)
    {
        ImFontBuildSrcDataFT& src_tmp = src_tmp_array[src_i];
        src_tmp.GlyphsList.reserve(src_tmp.GlyphsCount);

        IM_ASSERT(sizeof(src_tmp.GlyphsSet.Storage.Data[0]) == sizeof(ImU32));
        const ImU32* it_begin = src_tmp.GlyphsSet.Storage.begin();
        const ImU32* it_end = src_tmp.GlyphsSet.Storage.end();
        for (const ImU32* it = it_begin; it < it_end; it++)
            if (ImU32 entries_32 = *it)
                for (ImU32 bit_n = 0; bit_n < 32; bit_n++)
                    if (entries_32 & ((ImU32)1 << bit_n))
                    {
                        ImFontBuildSrcGlyph src_glyph;
                        src_glyph.Codepoint = (ImWchar)(((it - it_begin) << 5) + bit_n);
                        //src_glyph.GlyphIndex = 0; // FIXME-OPT: We had this info in the previous step and lost it..
                        src_tmp.GlyphsList.push_back(src_glyph);
                    }
        src_tmp.GlyphsSet.Clear();
        IM_ASSERT(src_tmp.GlyphsList.Size == src_tmp.GlyphsCount);
    }
    for (int dst_i = 0; dst_i < dst_tmp_array.Size; dst_i++)
        dst_tmp_array[dst_i].GlyphsSet.Clear();
    dst_tmp_array.clear();

    // Allocate packing character data and flag packed characters buffer as non-packed (x0=y0=x1=y1=0)
    // (We technically don't need to zero-clear buf_rects, but let's do it for the sake of sanity)
    ImVector<stbrp_rect> buf_rects;
    buf_rects.resize(total_glyphs_count);
    memset(buf_rects.Data, 0, (size_t)buf_rects.size_in_bytes());

    // Allocate temporary rasterization data buffers.
    // We could not find a way to retrieve accurate glyph size without rendering them.
    // (e.g. slot->metrics->width not always matching bitmap->width, especially considering the Oblique transform)
    // We allocate in chunks of 256 KB to not waste too much extra memory ahead. Hopefully users of FreeType won't find the temporary allocations.
    const int BITMAP_BUFFERS_CHUNK_SIZE = 256 * 1024;
    int buf_bitmap_current_used_bytes = 0;
    ImVector<unsigned char*> buf_bitmap_buffers;
    buf_bitmap_buffers.push_back((unsigned char*)IM_ALLOC(BITMAP_BUFFERS_CHUNK_SIZE));

    // 4. Gather glyphs sizes so we can pack them in our virtual canvas.
    // 8. Render/rasterize font characters into the texture
    int total_surface = 0;
    int buf_rects_out_n = 0;
    for (int src_i = 0; src_i < src_tmp_array.Size; src_i++)
    {
        ImFontBuildSrcDataFT& src_tmp = src_tmp_array[src_i];
        ImFontConfig& cfg = atlas->ConfigData[src_i];
        if (src_tmp.GlyphsCount == 0)
            continue;

        src_tmp.Rects = &buf_rects[buf_rects_out_n];
        buf_rects_out_n += src_tmp.GlyphsCount;

        // Compute multiply table if requested
        const bool multiply_enabled = (cfg.RasterizerMultiply != 1.0f);
        unsigned char multiply_table[256];
        if (multiply_enabled)
            ImFontAtlasBuildMultiplyCalcLookupTable(multiply_table, cfg.RasterizerMultiply);

        const int padding = atlas->TexGlyphPadding + 2 * IMGUI_SDF_PADDING + 1;
        if (cfg.FontPixels)
        {
			auto &Rects     = src_tmp.Rects;
			auto &glyphList = src_tmp.GlyphsList;
			for (int glyph_i = 0; glyph_i < glyphList.size(); glyph_i++)
            {
				ImFontBuildSrcGlyph &src_glyph = glyphList[glyph_i];
				auto &glyph = src_tmp.GlyphMap->at(src_glyph.Codepoint);

                auto &info = src_glyph.Info;
                info.Width          = glyph->width;
                info.Height         = glyph->height;
                info.OffsetX        = glyph->x;
                info.OffsetY        = glyph->y;
                info.AdvanceX       = glyph->advance;
                info.IsColored      = false;
                info.pl             = glyph->planeBoundsLeft;
                info.pr             = glyph->planeBoundsRight;
                info.pb             = glyph->planeBoundsBottom;
                info.pt             = glyph->planeBoundsTop;
				src_glyph.BitmapData = (uint32_t *)(src_tmp.pixels + 4 * (src_tmp.width * (src_tmp.bottom ? (src_tmp.height - glyph->y - glyph->height) : glyph->y) + glyph->x));

                Rects[glyph_i].w = glyph->width  + padding;
				Rects[glyph_i].h = glyph->height + padding;
                total_surface += Rects[glyph_i].w * Rects[glyph_i].h;
            }
        }
        else
        {
			MsdfgenFontBuildGlyph(src_tmp.Font, IMGUI_SDF_DETAIL);
			MsdfgenFontFillGlyphInfo(src_tmp.Font, src_tmp.GlyphsList.Data, src_tmp.GlyphsList.Size, (MsdfgenRect *) src_tmp.Rects, total_surface, padding, src_tmp.pixels, src_tmp.width, src_tmp.height);
        }
    }

    // We need a width for the skyline algorithm, any width!
    // The exact width doesn't really matter much, but some API/GPU have texture size limitations and increasing width can decrease height.
    // User can override TexDesiredWidth and TexGlyphPadding if they wish, otherwise we use a simple heuristic to select the width based on expected surface.
    const int surface_sqrt = (int)ImSqrt((float)total_surface) + 1;
    atlas->TexHeight = 0;
    if (atlas->TexDesiredWidth > 0)
        atlas->TexWidth = atlas->TexDesiredWidth;
    else
    {
        if (surface_sqrt >= 16384 * 0.7f)
        {
			atlas->TexWidth = 16384;
        }
        else if (surface_sqrt >= 8192 * 0.7f)
        {
			atlas->TexWidth = 8192;
        }
        else if (surface_sqrt >= 4096 * 0.7f)
        {
			atlas->TexWidth = 4096;
        }
        else if (surface_sqrt >= 2048 * 0.7f)
        {
			atlas->TexWidth = 2048;
        }
        else if (surface_sqrt >= 1024 * 0.7f)
        {
			atlas->TexWidth = 1024;
        }
        else
        {
			atlas->TexWidth = 512;
        }
    }

    // 5. Start packing
    // Pack our extra data rectangles first, so it will be on the upper-left corner of our texture (UV will have small values).
    const int TEX_HEIGHT_MAX = 1024 * 32;
    const int num_nodes_for_packing_algorithm = atlas->TexWidth - atlas->TexGlyphPadding - (2 * IMGUI_SDF_PADDING + 1);
    ImVector<stbrp_node> pack_nodes;
    pack_nodes.resize(num_nodes_for_packing_algorithm);
    stbrp_context pack_context;
    stbrp_init_target(&pack_context, atlas->TexWidth, TEX_HEIGHT_MAX, pack_nodes.Data, pack_nodes.Size);
    ImFontAtlasBuildPackCustomRects(atlas, &pack_context);

    // 6. Pack each source font. No rendering yet, we are working with rectangles in an infinitely tall texture at this point.
    for (int src_i = 0; src_i < src_tmp_array.Size; src_i++)
    {
        ImFontBuildSrcDataFT& src_tmp = src_tmp_array[src_i];
        if (src_tmp.GlyphsCount == 0)
            continue;

        stbrp_pack_rects(&pack_context, src_tmp.Rects, src_tmp.GlyphsCount);

        // Extend texture height and mark missing glyphs as non-packed so we won't render them.
        // FIXME: We are not handling packing failure here (would happen if we got off TEX_HEIGHT_MAX or if a single if larger than TexWidth?)
        for (int glyph_i = 0; glyph_i < src_tmp.GlyphsCount; glyph_i++)
            if (src_tmp.Rects[glyph_i].was_packed)
                atlas->TexHeight = ImMax(atlas->TexHeight, src_tmp.Rects[glyph_i].y + src_tmp.Rects[glyph_i].h);
    }

    // 7. Allocate texture
    atlas->TexHeight =  (atlas->Flags & ImFontAtlasFlags_NoPowerOfTwoHeight) ? (atlas->TexHeight + 1) : ImUpperPowerOfTwo(atlas->TexHeight);
    atlas->TexUvScale = ImVec2(1.0f / atlas->TexWidth, 1.0f / atlas->TexHeight);
    if (src_load_color)
    {
        size_t tex_size = (size_t)atlas->TexWidth * atlas->TexHeight * 4;
        atlas->TexPixelsRGBA32 = (unsigned int*)IM_ALLOC(tex_size);
        memset(atlas->TexPixelsRGBA32, 0, tex_size);
    }
    else
    {
        size_t tex_size = (size_t)atlas->TexWidth * atlas->TexHeight * 1;
        atlas->TexPixelsAlpha8 = (unsigned char*)IM_ALLOC(tex_size);
        memset(atlas->TexPixelsAlpha8, 0, tex_size);
    }

    // 8. Copy rasterized font characters back into the main texture
    // 9. Setup ImFont and glyphs for runtime
    bool tex_use_colors = false;

    for (int src_i = 0; src_i < src_tmp_array.Size; src_i++)
    {
        ImFontBuildSrcDataFT& src_tmp = src_tmp_array[src_i];
        if (src_tmp.GlyphsCount == 0)
            continue;

        // When merging fonts with MergeMode=true:
        // - We can have multiple input fonts writing into a same destination font.
        // - dst_font->ConfigData is != from cfg which is our source configuration.
        ImFontConfig& cfg = atlas->ConfigData[src_i];
        ImFont* dst_font = cfg.DstFont;

        MsdfgenFontMetrics metrics{};
		if (src_tmp.Metrics)
		{
			metrics = *src_tmp.Metrics;
		}
        else
        {
			metrics = MsdfgenFontGetFontMetrics(src_tmp.Font);
        }
        double unscaled_ascent   = metrics.ascenderY;
        double unscaled_descent  = metrics.descenderY;
        double unscaled_line_gap = metrics.lineHeight;

        const float ascent  = unscaled_ascent  * cfg.SizePixels;
        const float descent = unscaled_descent * cfg.SizePixels;

        ImFontAtlasBuildSetupFont(atlas, dst_font, &cfg, ascent, descent);

        float font_off_x = cfg.GlyphOffset.x;
        float font_off_y = cfg.GlyphOffset.y + IM_ROUND(dst_font->Ascent);
		font_off_x = cfg.GlyphOffset.x / IMGUI_SDF_DETAIL;
		font_off_y = cfg.GlyphOffset.y / IMGUI_SDF_DETAIL;

        float offset = (metrics.lineHeight * IMGUI_SDF_DETAIL - IMGUI_SDF_DETAIL) / IMGUI_SDF_DETAIL * 0.5f;
		font_off_x += offset;
		font_off_y += offset;

        const int padding = atlas->TexGlyphPadding + IMGUI_SDF_PADDING;

        font_off_y = 0;
   //     if (cfg.GlyphMinAdvanceX > 0.0f)
   //     {
			//font_off_y = (cfg.GlyphMinAdvanceX - cfg.SizePixels) / IMGUI_SDF_DETAIL;
   //     }
        for (int glyph_i = 0; glyph_i < src_tmp.GlyphsCount; glyph_i++)
        {
            ImFontBuildSrcGlyph& src_glyph = src_tmp.GlyphsList[glyph_i];
            stbrp_rect& pack_rect = src_tmp.Rects[glyph_i];
            IM_ASSERT(pack_rect.was_packed);
            if (pack_rect.w == 0 && pack_rect.h == 0)
                continue;

            GlyphInfo& info = src_glyph.Info;
            IM_ASSERT(info.Width + padding <= pack_rect.w);
            IM_ASSERT(info.Height + padding <= pack_rect.h);
            const int tx = pack_rect.x + padding;
            const int ty = pack_rect.y + padding;

            auto &pl = info.pl;
            auto &pr = info.pr;
            auto &pb = info.pb;
            auto &pt = info.pt;

            float width  = pr - pl;
            float height = pt - pb;

			float x0 = pl /*+ font_off_x*/;
			float y0 = metrics.ascenderY + /*metrics.descenderY*/ - pt + font_off_y;
            float x1 = x0 + width;
            float y1 = y0 + height;

            float u0 = (tx) / (float)atlas->TexWidth;
            float v0 = (ty) / (float)atlas->TexHeight;
            float u1 = (tx + info.Width)  / (float)atlas->TexWidth;
            float v1 = (ty + info.Height) / (float)atlas->TexHeight;
            float u_padding  = float(IMGUI_SDF_PADDING) / atlas->TexWidth;
            float v_padding  = float(IMGUI_SDF_PADDING) / atlas->TexHeight;
            float xy_padding = float(IMGUI_SDF_PADDING) / IMGUI_SDF_DETAIL;
			dst_font->AddGlyph(&cfg, (ImWchar) src_glyph.Codepoint, x0 - xy_padding, y0 - xy_padding, x1 + xy_padding, y1 + xy_padding, u0 - u_padding, v0 - v_padding, u1 + u_padding, v1 + v_padding, info.AdvanceX, cfg.SizePixels / metrics.lineHeight);

            ImFontGlyph* dst_glyph = &dst_font->Glyphs.back();
            IM_ASSERT(dst_glyph->Codepoint == src_glyph.Codepoint);
            if (src_glyph.Info.IsColored)
                dst_glyph->Colored = tex_use_colors = true;

            if (src_tmp.bottom)
            {
				size_t blit_src_stride = src_tmp.width * 4;
				size_t blit_dst_stride = (size_t) atlas->TexWidth;
				uint8_t *blit_src = (uint8_t *) src_glyph.BitmapData;
				unsigned int *blit_dst = atlas->TexPixelsRGBA32 + (ty * blit_dst_stride) + tx;
				for (int y = 0; y < info.Height; y++, blit_dst += blit_dst_stride, blit_src += blit_src_stride)
				{
					for (int x = 0; x < info.Width; x++)
					{
						auto src = &blit_src[x * sourceChannels];
						blit_dst[x] = IM_COL32(src[0], src[1], src[2], src[3]);
					}
				}
				continue;
            }
            // Blit from temporary buffer to final texture
			size_t blit_src_stride = src_tmp.width * sourceChannels;        // (size_t)src_glyph.Info.Width;
            size_t blit_dst_stride = (size_t)atlas->TexWidth;
            uint8_t *blit_src = (uint8_t *)src_glyph.BitmapData + blit_src_stride * (info.Height - 1);
            if (atlas->TexPixelsAlpha8 != NULL)
            {
                unsigned char* blit_dst = atlas->TexPixelsAlpha8 + (ty * blit_dst_stride) + tx;
                for (int y = 0; y < info.Height; y++, blit_dst += blit_dst_stride, blit_src -= blit_src_stride)
                    for (int x = 0; x < info.Width; x++)
                    {
						auto src = &blit_src[x * sourceChannels];
                        blit_dst[x] = (unsigned char)((src[x] >> IM_COL32_A_SHIFT) & 0xFF);
                    }
            }
            else
            {
                unsigned int* blit_dst = atlas->TexPixelsRGBA32 + (ty * blit_dst_stride) + tx;
                if (numChannels == 1)
                {
                    for (int y = 0; y < info.Height; y++, blit_dst += blit_dst_stride, blit_src -= blit_src_stride)
                    {
                        for (int x = 0; x < info.Width; x++)
                        {
							auto src = &blit_src[x * sourceChannels];
                            blit_dst[x] = IM_COL32(255, 255, 255, src[0]);
                        }
                    }
                }
                else if (numChannels == 3)
                {
                    for (int y = 0; y < info.Height; y++, blit_dst += blit_dst_stride, blit_src -= blit_src_stride)
                        for (int x = 0; x < info.Width; x++)
                        {
							auto src = &blit_src[x * sourceChannels];
                            blit_dst[x] = IM_COL32(src[0], src[1], src[2], 255);
                        }
                }
                else if (numChannels == 4)
                {
                    for (int y = 0; y < info.Height; y++, blit_dst += blit_dst_stride, blit_src -= blit_src_stride)
                    {
                        for (int x = 0; x < info.Width; x++)
                        {
							auto src = &blit_src[x * sourceChannels];
                            blit_dst[x] = IM_COL32(src[0], src[1], src[2], src[3]);
                        }
                    }
                }
            }
        }

        src_tmp.Rects = NULL;
		MsdfgenHandleDestroyMsdfgenFont(handle, &src_tmp.Font);
    }
    atlas->TexPixelsUseColors = tex_use_colors;

    // Cleanup
    for (int buf_i = 0; buf_i < buf_bitmap_buffers.Size; buf_i++)
        IM_FREE(buf_bitmap_buffers[buf_i]);
    src_tmp_array.clear_destruct();

    ImFontAtlasBuildFinish(atlas);

    return true;
}


static bool ImFontAtlasBuildWithMsdfgen(ImFontAtlas *atlas, ImDispatch dispatcher)
{
    MsdfgenHandle *handle = CreateMsdfgenHandle();
    if (!handle)
    {
        return false;
    }

    bool ret = ImFontAtlasBuildWithMsdfgen(handle, atlas, atlas->FontBuilderFlags);
    DestroyMsdfgenHandle(&handle);

    return ret;
}

const ImFontBuilderIO* ImGuiMsdfgen::GetBuilderForMsdfgen()
{
    static ImFontBuilderIO io;
    io.FontBuilder_Build = ImFontAtlasBuildWithMsdfgen;
    return &io;
}

void SetAllocatorFunctions(void* (*alloc_func)(size_t sz, void* user_data), void (*free_func)(void* ptr, void* user_data), void* user_data)
{

}

}
