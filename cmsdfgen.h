#pragma once

#include <cstdint>
#include <cstring>

#ifdef _MSC_VER
#	if defined EXPORT_CMSDFGEN
#		define CMSDFGEN_API __declspec(dllexport)
#	elif defined IMPORT_CMSDFGEN
#		define CMSDFGEN_API __declspec(dllimport)
#	else
#		define CMSDFGEN_API
#	endif
#elif defined(__GNUC__) || defined(__clang__)
#	define CMSDFGEN_API __attribute__((visibility("default")))
#else
#	error Unsupport Platform Detected!
#endif

class MsdfgenHandle;
extern "C" CMSDFGEN_API MsdfgenHandle *CreateMsdfgenHandle();

extern "C" CMSDFGEN_API void DestroyMsdfgenHandle(MsdfgenHandle **ppHandle);

enum class MSDFType
{
	SDF,
	PSDF,
	MSDF,
	MTSDF,
};

#ifdef STBRP_LARGE_RECTS
typedef int msdfgen_coord;
#else
typedef unsigned short msdfgen_coord;
#endif

struct GlyphInfo
{
	int Width;             // Glyph's width in pixels.
	int Height;            // Glyph's height in pixels.
	int OffsetX;           // The distance from the origin ("pen position") to the left of the glyph.
	int OffsetY;           // The distance from the origin to the top of the glyph. This is usually a value < 0.
	float AdvanceX;        // The distance from the origin to the origin of the next glyph. This is usually a value > 0.
	bool IsColored;        // The glyph is colored
	double pl;
	double pt;
	double pr;
	double pb;
};

struct ImFontBuildSrcGlyph
{
	GlyphInfo Info;
	uint32_t Codepoint;
	unsigned int *BitmapData;        // Point within one of the dst_tmp_bitmap_buffers[] array

	ImFontBuildSrcGlyph()
	{
		memset(this, 0, sizeof(*this));
	}
};

struct MsdfgenRect
{
	// reserved for your use:
	int id;

	// input:
	msdfgen_coord w, h;

	// output:
	msdfgen_coord x, y;
	int was_packed;        // non-zero if valid packing

};        // 16 bytes, nominally

/// Global metrics of a typeface (in font units).
struct MsdfgenFontMetrics
{
	double emSize;
	double ascenderY;
	double descenderY;
	double lineHeight;
	double underlineY;
	double underlineThickness;
};

struct MsdfgenGlyphRagen
{
	wchar_t begin;
	wchar_t end;
};

struct MsdfgenAtlasData
{
	char type[8];
	uint8_t distanceRange;
	uint8_t distanceRangeMiddle;
	float size;
	uint32_t width;
	uint32_t height;
	char yOrigin[8];
};

struct MsdfgenGlyphData
{
	uint32_t codepoint;
	uint32_t x;
	uint32_t y;
	uint32_t width;
	uint32_t height;
	float advance;
	float planeBoundsLeft;
	float planeBoundsBottom;
	float planeBoundsRight;
	float planeBoundsTop;
};

class MsdfgenFont;
extern "C" CMSDFGEN_API MsdfgenFont *MsdfgenHandleCreateMsdfgenFont(MsdfgenHandle *handle, const uint8_t *fontData, uint32_t fontDataSize, MSDFType type);

extern "C" CMSDFGEN_API void MsdfgenHandleDestroyMsdfgenFont(MsdfgenHandle *handle, MsdfgenFont **ppFont);

extern "C" CMSDFGEN_API int MsdfgenFontGetGlyphIndex(MsdfgenFont *handle, int codepoint);

extern "C" CMSDFGEN_API void MsdfgenFontBuildGlyph(MsdfgenFont *handle, float fontSize);

extern "C" CMSDFGEN_API void MsdfgenFontFillGlyphInfo(MsdfgenFont *handle, ImFontBuildSrcGlyph *srcGlyphs, int glyphSize, MsdfgenRect *rects, int &total_surface, int padding, uint8_t *(&pixels), int &width, int &heigth);

extern "C" CMSDFGEN_API MsdfgenFontMetrics MsdfgenFontGetFontMetrics(MsdfgenFont *handle);

extern "C" CMSDFGEN_API int MsdfgenConvertFont(const char *fontPath, const char *outputPath, MSDFType type, MsdfgenGlyphRagen *ranges);
