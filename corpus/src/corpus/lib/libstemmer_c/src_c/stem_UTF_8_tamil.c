/* This file was generated automatically by the Snowball to ISO C compiler */
/* http://snowballstem.org/ */

#include "../runtime/header.h"

#ifdef __cplusplus
extern "C" {
#endif
extern int tamil_UTF_8_stem(struct SN_env * z);
#ifdef __cplusplus
}
#endif
static int r_has_min_length(struct SN_env * z);
static int r_remove_common_word_endings(struct SN_env * z);
static int r_remove_tense_suffixes(struct SN_env * z);
static int r_remove_tense_suffix(struct SN_env * z);
static int r_fix_endings(struct SN_env * z);
static int r_fix_ending(struct SN_env * z);
static int r_fix_va_start(struct SN_env * z);
static int r_remove_vetrumai_urupukal(struct SN_env * z);
static int r_remove_um(struct SN_env * z);
static int r_remove_command_suffixes(struct SN_env * z);
static int r_remove_pronoun_prefixes(struct SN_env * z);
static int r_remove_question_prefixes(struct SN_env * z);
static int r_remove_question_suffixes(struct SN_env * z);
static int r_remove_plural_suffix(struct SN_env * z);
#ifdef __cplusplus
extern "C" {
#endif


extern struct SN_env * tamil_UTF_8_create_env(void);
extern void tamil_UTF_8_close_env(struct SN_env * z);


#ifdef __cplusplus
}
#endif
static const symbol s_0_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_0_1[3] = { 0xE0, 0xAE, 0x99 };
static const symbol s_0_2[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_0_3[3] = { 0xE0, 0xAE, 0x9E };
static const symbol s_0_4[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_0_5[3] = { 0xE0, 0xAE, 0xA8 };
static const symbol s_0_6[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_0_7[3] = { 0xE0, 0xAE, 0xAE };
static const symbol s_0_8[3] = { 0xE0, 0xAE, 0xAF };
static const symbol s_0_9[3] = { 0xE0, 0xAE, 0xB5 };

static const struct among a_0[10] =
{
/*  0 */ { 3, s_0_0, -1, -1, 0},
/*  1 */ { 3, s_0_1, -1, -1, 0},
/*  2 */ { 3, s_0_2, -1, -1, 0},
/*  3 */ { 3, s_0_3, -1, -1, 0},
/*  4 */ { 3, s_0_4, -1, -1, 0},
/*  5 */ { 3, s_0_5, -1, -1, 0},
/*  6 */ { 3, s_0_6, -1, -1, 0},
/*  7 */ { 3, s_0_7, -1, -1, 0},
/*  8 */ { 3, s_0_8, -1, -1, 0},
/*  9 */ { 3, s_0_9, -1, -1, 0}
};

static const symbol s_1_0[12] = { 0xE0, 0xAE, 0xA8, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x8D };
static const symbol s_1_1[6] = { 0xE0, 0xAE, 0xA8, 0xE0, 0xAF, 0x8D };
static const symbol s_1_2[9] = { 0xE0, 0xAE, 0xA8, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xA4 };

static const struct among a_1[3] =
{
/*  0 */ { 12, s_1_0, -1, -1, 0},
/*  1 */ { 6, s_1_1, -1, -1, 0},
/*  2 */ { 9, s_1_2, -1, -1, 0}
};

static const symbol s_2_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_2_1[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_2_2[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_2[3] =
{
/*  0 */ { 3, s_2_0, -1, -1, 0},
/*  1 */ { 3, s_2_1, -1, -1, 0},
/*  2 */ { 3, s_2_2, -1, -1, 0}
};

static const symbol s_3_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_3_1[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_3_2[3] = { 0xE0, 0xAE, 0x9F };
static const symbol s_3_3[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_3_4[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_3_5[3] = { 0xE0, 0xAE, 0xB1 };

static const struct among a_3[6] =
{
/*  0 */ { 3, s_3_0, -1, -1, 0},
/*  1 */ { 3, s_3_1, -1, -1, 0},
/*  2 */ { 3, s_3_2, -1, -1, 0},
/*  3 */ { 3, s_3_3, -1, -1, 0},
/*  4 */ { 3, s_3_4, -1, -1, 0},
/*  5 */ { 3, s_3_5, -1, -1, 0}
};

static const symbol s_4_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_4_1[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_4_2[3] = { 0xE0, 0xAE, 0x9F };
static const symbol s_4_3[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_4_4[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_4_5[3] = { 0xE0, 0xAE, 0xB1 };

static const struct among a_4[6] =
{
/*  0 */ { 3, s_4_0, -1, -1, 0},
/*  1 */ { 3, s_4_1, -1, -1, 0},
/*  2 */ { 3, s_4_2, -1, -1, 0},
/*  3 */ { 3, s_4_3, -1, -1, 0},
/*  4 */ { 3, s_4_4, -1, -1, 0},
/*  5 */ { 3, s_4_5, -1, -1, 0}
};

static const symbol s_5_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_5_1[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_5_2[3] = { 0xE0, 0xAE, 0x9F };
static const symbol s_5_3[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_5_4[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_5_5[3] = { 0xE0, 0xAE, 0xB1 };

static const struct among a_5[6] =
{
/*  0 */ { 3, s_5_0, -1, -1, 0},
/*  1 */ { 3, s_5_1, -1, -1, 0},
/*  2 */ { 3, s_5_2, -1, -1, 0},
/*  3 */ { 3, s_5_3, -1, -1, 0},
/*  4 */ { 3, s_5_4, -1, -1, 0},
/*  5 */ { 3, s_5_5, -1, -1, 0}
};

static const symbol s_6_0[3] = { 0xE0, 0xAE, 0xAF };
static const symbol s_6_1[3] = { 0xE0, 0xAE, 0xB0 };
static const symbol s_6_2[3] = { 0xE0, 0xAE, 0xB2 };
static const symbol s_6_3[3] = { 0xE0, 0xAE, 0xB3 };
static const symbol s_6_4[3] = { 0xE0, 0xAE, 0xB4 };
static const symbol s_6_5[3] = { 0xE0, 0xAE, 0xB5 };

static const struct among a_6[6] =
{
/*  0 */ { 3, s_6_0, -1, -1, 0},
/*  1 */ { 3, s_6_1, -1, -1, 0},
/*  2 */ { 3, s_6_2, -1, -1, 0},
/*  3 */ { 3, s_6_3, -1, -1, 0},
/*  4 */ { 3, s_6_4, -1, -1, 0},
/*  5 */ { 3, s_6_5, -1, -1, 0}
};

static const symbol s_7_0[3] = { 0xE0, 0xAE, 0x99 };
static const symbol s_7_1[3] = { 0xE0, 0xAE, 0x9E };
static const symbol s_7_2[3] = { 0xE0, 0xAE, 0xA3 };
static const symbol s_7_3[3] = { 0xE0, 0xAE, 0xA8 };
static const symbol s_7_4[3] = { 0xE0, 0xAE, 0xA9 };
static const symbol s_7_5[3] = { 0xE0, 0xAE, 0xAE };

static const struct among a_7[6] =
{
/*  0 */ { 3, s_7_0, -1, -1, 0},
/*  1 */ { 3, s_7_1, -1, -1, 0},
/*  2 */ { 3, s_7_2, -1, -1, 0},
/*  3 */ { 3, s_7_3, -1, -1, 0},
/*  4 */ { 3, s_7_4, -1, -1, 0},
/*  5 */ { 3, s_7_5, -1, -1, 0}
};

static const symbol s_8_0[6] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x8D };
static const symbol s_8_1[3] = { 0xE0, 0xAE, 0xAF };
static const symbol s_8_2[3] = { 0xE0, 0xAE, 0xB5 };

static const struct among a_8[3] =
{
/*  0 */ { 6, s_8_0, -1, -1, 0},
/*  1 */ { 3, s_8_1, -1, -1, 0},
/*  2 */ { 3, s_8_2, -1, -1, 0}
};

static const symbol s_9_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_9_1[3] = { 0xE0, 0xAF, 0x81 };
static const symbol s_9_2[3] = { 0xE0, 0xAF, 0x82 };
static const symbol s_9_3[3] = { 0xE0, 0xAF, 0x86 };
static const symbol s_9_4[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_9_5[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_9_6[3] = { 0xE0, 0xAE, 0xBE };
static const symbol s_9_7[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_9[8] =
{
/*  0 */ { 3, s_9_0, -1, -1, 0},
/*  1 */ { 3, s_9_1, -1, -1, 0},
/*  2 */ { 3, s_9_2, -1, -1, 0},
/*  3 */ { 3, s_9_3, -1, -1, 0},
/*  4 */ { 3, s_9_4, -1, -1, 0},
/*  5 */ { 3, s_9_5, -1, -1, 0},
/*  6 */ { 3, s_9_6, -1, -1, 0},
/*  7 */ { 3, s_9_7, -1, -1, 0}
};

static const symbol s_10_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_10_1[3] = { 0xE0, 0xAF, 0x81 };
static const symbol s_10_2[3] = { 0xE0, 0xAF, 0x82 };
static const symbol s_10_3[3] = { 0xE0, 0xAF, 0x86 };
static const symbol s_10_4[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_10_5[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_10_6[3] = { 0xE0, 0xAE, 0xBE };
static const symbol s_10_7[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_10[8] =
{
/*  0 */ { 3, s_10_0, -1, -1, 0},
/*  1 */ { 3, s_10_1, -1, -1, 0},
/*  2 */ { 3, s_10_2, -1, -1, 0},
/*  3 */ { 3, s_10_3, -1, -1, 0},
/*  4 */ { 3, s_10_4, -1, -1, 0},
/*  5 */ { 3, s_10_5, -1, -1, 0},
/*  6 */ { 3, s_10_6, -1, -1, 0},
/*  7 */ { 3, s_10_7, -1, -1, 0}
};

static const symbol s_11_0[3] = { 0xE0, 0xAE, 0x85 };
static const symbol s_11_1[3] = { 0xE0, 0xAE, 0x87 };
static const symbol s_11_2[3] = { 0xE0, 0xAE, 0x89 };

static const struct among a_11[3] =
{
/*  0 */ { 3, s_11_0, -1, -1, 0},
/*  1 */ { 3, s_11_1, -1, -1, 0},
/*  2 */ { 3, s_11_2, -1, -1, 0}
};

static const symbol s_12_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_12_1[3] = { 0xE0, 0xAE, 0x99 };
static const symbol s_12_2[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_12_3[3] = { 0xE0, 0xAE, 0x9E };
static const symbol s_12_4[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_12_5[3] = { 0xE0, 0xAE, 0xA8 };
static const symbol s_12_6[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_12_7[3] = { 0xE0, 0xAE, 0xAE };
static const symbol s_12_8[3] = { 0xE0, 0xAE, 0xAF };
static const symbol s_12_9[3] = { 0xE0, 0xAE, 0xB5 };

static const struct among a_12[10] =
{
/*  0 */ { 3, s_12_0, -1, -1, 0},
/*  1 */ { 3, s_12_1, -1, -1, 0},
/*  2 */ { 3, s_12_2, -1, -1, 0},
/*  3 */ { 3, s_12_3, -1, -1, 0},
/*  4 */ { 3, s_12_4, -1, -1, 0},
/*  5 */ { 3, s_12_5, -1, -1, 0},
/*  6 */ { 3, s_12_6, -1, -1, 0},
/*  7 */ { 3, s_12_7, -1, -1, 0},
/*  8 */ { 3, s_12_8, -1, -1, 0},
/*  9 */ { 3, s_12_9, -1, -1, 0}
};

static const symbol s_13_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_13_1[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_13_2[3] = { 0xE0, 0xAE, 0x9F };
static const symbol s_13_3[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_13_4[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_13_5[3] = { 0xE0, 0xAE, 0xB1 };

static const struct among a_13[6] =
{
/*  0 */ { 3, s_13_0, -1, -1, 0},
/*  1 */ { 3, s_13_1, -1, -1, 0},
/*  2 */ { 3, s_13_2, -1, -1, 0},
/*  3 */ { 3, s_13_3, -1, -1, 0},
/*  4 */ { 3, s_13_4, -1, -1, 0},
/*  5 */ { 3, s_13_5, -1, -1, 0}
};

static const symbol s_14_0[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_14_1[3] = { 0xE0, 0xAF, 0x8B };
static const symbol s_14_2[3] = { 0xE0, 0xAE, 0xBE };

static const struct among a_14[3] =
{
/*  0 */ { 3, s_14_0, -1, -1, 0},
/*  1 */ { 3, s_14_1, -1, -1, 0},
/*  2 */ { 3, s_14_2, -1, -1, 0}
};

static const symbol s_15_0[6] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xBF };
static const symbol s_15_1[6] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xBF };

static const struct among a_15[2] =
{
/*  0 */ { 6, s_15_0, -1, -1, 0},
/*  1 */ { 6, s_15_1, -1, -1, 0}
};

static const symbol s_16_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_16_1[3] = { 0xE0, 0xAF, 0x81 };
static const symbol s_16_2[3] = { 0xE0, 0xAF, 0x82 };
static const symbol s_16_3[3] = { 0xE0, 0xAF, 0x86 };
static const symbol s_16_4[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_16_5[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_16_6[3] = { 0xE0, 0xAE, 0xBE };
static const symbol s_16_7[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_16[8] =
{
/*  0 */ { 3, s_16_0, -1, -1, 0},
/*  1 */ { 3, s_16_1, -1, -1, 0},
/*  2 */ { 3, s_16_2, -1, -1, 0},
/*  3 */ { 3, s_16_3, -1, -1, 0},
/*  4 */ { 3, s_16_4, -1, -1, 0},
/*  5 */ { 3, s_16_5, -1, -1, 0},
/*  6 */ { 3, s_16_6, -1, -1, 0},
/*  7 */ { 3, s_16_7, -1, -1, 0}
};

static const symbol s_17_0[15] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_17_1[18] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_17_2[9] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_17_3[12] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_17_4[18] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81 };
static const symbol s_17_5[21] = { 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB2, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_17_6[12] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F };
static const symbol s_17_7[15] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xA3 };
static const symbol s_17_8[9] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xA9 };
static const symbol s_17_9[18] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA4, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xA9 };
static const symbol s_17_10[15] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xB0, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xAF };
static const symbol s_17_11[9] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xBF };
static const symbol s_17_12[15] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAE, 0xBF };

static const struct among a_17[13] =
{
/*  0 */ { 15, s_17_0, -1, -1, 0},
/*  1 */ { 18, s_17_1, -1, -1, 0},
/*  2 */ { 9, s_17_2, -1, -1, 0},
/*  3 */ { 12, s_17_3, -1, -1, 0},
/*  4 */ { 18, s_17_4, -1, -1, 0},
/*  5 */ { 21, s_17_5, -1, -1, 0},
/*  6 */ { 12, s_17_6, -1, -1, 0},
/*  7 */ { 15, s_17_7, -1, -1, 0},
/*  8 */ { 9, s_17_8, -1, -1, 0},
/*  9 */ { 18, s_17_9, 8, -1, 0},
/* 10 */ { 15, s_17_10, -1, -1, 0},
/* 11 */ { 9, s_17_11, -1, -1, 0},
/* 12 */ { 15, s_17_12, -1, -1, 0}
};

static const symbol s_18_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_18_1[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_18_2[3] = { 0xE0, 0xAE, 0x9F };
static const symbol s_18_3[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_18_4[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_18_5[3] = { 0xE0, 0xAE, 0xB1 };

static const struct among a_18[6] =
{
/*  0 */ { 3, s_18_0, -1, -1, 0},
/*  1 */ { 3, s_18_1, -1, -1, 0},
/*  2 */ { 3, s_18_2, -1, -1, 0},
/*  3 */ { 3, s_18_3, -1, -1, 0},
/*  4 */ { 3, s_18_4, -1, -1, 0},
/*  5 */ { 3, s_18_5, -1, -1, 0}
};

static const symbol s_19_0[3] = { 0xE0, 0xAE, 0x95 };
static const symbol s_19_1[3] = { 0xE0, 0xAE, 0x9A };
static const symbol s_19_2[3] = { 0xE0, 0xAE, 0x9F };
static const symbol s_19_3[3] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_19_4[3] = { 0xE0, 0xAE, 0xAA };
static const symbol s_19_5[3] = { 0xE0, 0xAE, 0xB1 };

static const struct among a_19[6] =
{
/*  0 */ { 3, s_19_0, -1, -1, 0},
/*  1 */ { 3, s_19_1, -1, -1, 0},
/*  2 */ { 3, s_19_2, -1, -1, 0},
/*  3 */ { 3, s_19_3, -1, -1, 0},
/*  4 */ { 3, s_19_4, -1, -1, 0},
/*  5 */ { 3, s_19_5, -1, -1, 0}
};

static const symbol s_20_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_20_1[3] = { 0xE0, 0xAF, 0x81 };
static const symbol s_20_2[3] = { 0xE0, 0xAF, 0x82 };
static const symbol s_20_3[3] = { 0xE0, 0xAF, 0x86 };
static const symbol s_20_4[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_20_5[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_20_6[3] = { 0xE0, 0xAE, 0xBE };
static const symbol s_20_7[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_20[8] =
{
/*  0 */ { 3, s_20_0, -1, -1, 0},
/*  1 */ { 3, s_20_1, -1, -1, 0},
/*  2 */ { 3, s_20_2, -1, -1, 0},
/*  3 */ { 3, s_20_3, -1, -1, 0},
/*  4 */ { 3, s_20_4, -1, -1, 0},
/*  5 */ { 3, s_20_5, -1, -1, 0},
/*  6 */ { 3, s_20_6, -1, -1, 0},
/*  7 */ { 3, s_20_7, -1, -1, 0}
};

static const symbol s_21_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_21_1[3] = { 0xE0, 0xAF, 0x81 };
static const symbol s_21_2[3] = { 0xE0, 0xAF, 0x82 };
static const symbol s_21_3[3] = { 0xE0, 0xAF, 0x86 };
static const symbol s_21_4[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_21_5[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_21_6[3] = { 0xE0, 0xAE, 0xBE };
static const symbol s_21_7[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_21[8] =
{
/*  0 */ { 3, s_21_0, -1, -1, 0},
/*  1 */ { 3, s_21_1, -1, -1, 0},
/*  2 */ { 3, s_21_2, -1, -1, 0},
/*  3 */ { 3, s_21_3, -1, -1, 0},
/*  4 */ { 3, s_21_4, -1, -1, 0},
/*  5 */ { 3, s_21_5, -1, -1, 0},
/*  6 */ { 3, s_21_6, -1, -1, 0},
/*  7 */ { 3, s_21_7, -1, -1, 0}
};

static const symbol s_22_0[9] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_22_1[24] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8A, 0xE0, 0xAE, 0xA3, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };

static const struct among a_22[2] =
{
/*  0 */ { 9, s_22_0, -1, -1, 0},
/*  1 */ { 24, s_22_1, -1, -1, 0}
};

static const symbol s_23_0[3] = { 0xE0, 0xAE, 0x85 };
static const symbol s_23_1[3] = { 0xE0, 0xAE, 0x86 };
static const symbol s_23_2[3] = { 0xE0, 0xAE, 0x87 };
static const symbol s_23_3[3] = { 0xE0, 0xAE, 0x88 };
static const symbol s_23_4[3] = { 0xE0, 0xAE, 0x89 };
static const symbol s_23_5[3] = { 0xE0, 0xAE, 0x8A };
static const symbol s_23_6[3] = { 0xE0, 0xAE, 0x8E };
static const symbol s_23_7[3] = { 0xE0, 0xAE, 0x8F };
static const symbol s_23_8[3] = { 0xE0, 0xAE, 0x90 };
static const symbol s_23_9[3] = { 0xE0, 0xAE, 0x92 };
static const symbol s_23_10[3] = { 0xE0, 0xAE, 0x93 };
static const symbol s_23_11[3] = { 0xE0, 0xAE, 0x94 };

static const struct among a_23[12] =
{
/*  0 */ { 3, s_23_0, -1, -1, 0},
/*  1 */ { 3, s_23_1, -1, -1, 0},
/*  2 */ { 3, s_23_2, -1, -1, 0},
/*  3 */ { 3, s_23_3, -1, -1, 0},
/*  4 */ { 3, s_23_4, -1, -1, 0},
/*  5 */ { 3, s_23_5, -1, -1, 0},
/*  6 */ { 3, s_23_6, -1, -1, 0},
/*  7 */ { 3, s_23_7, -1, -1, 0},
/*  8 */ { 3, s_23_8, -1, -1, 0},
/*  9 */ { 3, s_23_9, -1, -1, 0},
/* 10 */ { 3, s_23_10, -1, -1, 0},
/* 11 */ { 3, s_23_11, -1, -1, 0}
};

static const symbol s_24_0[3] = { 0xE0, 0xAF, 0x80 };
static const symbol s_24_1[3] = { 0xE0, 0xAF, 0x81 };
static const symbol s_24_2[3] = { 0xE0, 0xAF, 0x82 };
static const symbol s_24_3[3] = { 0xE0, 0xAF, 0x86 };
static const symbol s_24_4[3] = { 0xE0, 0xAF, 0x87 };
static const symbol s_24_5[3] = { 0xE0, 0xAF, 0x88 };
static const symbol s_24_6[3] = { 0xE0, 0xAE, 0xBE };
static const symbol s_24_7[3] = { 0xE0, 0xAE, 0xBF };

static const struct among a_24[8] =
{
/*  0 */ { 3, s_24_0, -1, -1, 0},
/*  1 */ { 3, s_24_1, -1, -1, 0},
/*  2 */ { 3, s_24_2, -1, -1, 0},
/*  3 */ { 3, s_24_3, -1, -1, 0},
/*  4 */ { 3, s_24_4, -1, -1, 0},
/*  5 */ { 3, s_24_5, -1, -1, 0},
/*  6 */ { 3, s_24_6, -1, -1, 0},
/*  7 */ { 3, s_24_7, -1, -1, 0}
};

static const symbol s_25_0[18] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D };
static const symbol s_25_1[21] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xA8, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D };
static const symbol s_25_2[12] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D };
static const symbol s_25_3[15] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1 };
static const symbol s_25_4[18] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xA8, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1 };
static const symbol s_25_5[9] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB1 };

static const struct among a_25[6] =
{
/*  0 */ { 18, s_25_0, -1, -1, 0},
/*  1 */ { 21, s_25_1, -1, -1, 0},
/*  2 */ { 12, s_25_2, -1, -1, 0},
/*  3 */ { 15, s_25_3, -1, -1, 0},
/*  4 */ { 18, s_25_4, -1, -1, 0},
/*  5 */ { 9, s_25_5, -1, -1, 0}
};

static const symbol s_0[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x8B };
static const symbol s_1[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x8B };
static const symbol s_2[] = { 0xE0, 0xAE, 0x93 };
static const symbol s_3[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x8A };
static const symbol s_4[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x8A };
static const symbol s_5[] = { 0xE0, 0xAE, 0x92 };
static const symbol s_6[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x81 };
static const symbol s_7[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x81 };
static const symbol s_8[] = { 0xE0, 0xAE, 0x89 };
static const symbol s_9[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x82 };
static const symbol s_10[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x82 };
static const symbol s_11[] = { 0xE0, 0xAE, 0x8A };
static const symbol s_12[] = { 0xE0, 0xAE, 0x8E };
static const symbol s_13[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_14[] = { 0xE0, 0xAE, 0xAF, 0xE0, 0xAF, 0x8D };
static const symbol s_15[] = { 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xAA, 0xE0, 0xAF, 0x8D };
static const symbol s_16[] = { 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8D };
static const symbol s_17[] = { 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_18[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D };
static const symbol s_19[] = { 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_20[] = { 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8D };
static const symbol s_21[] = { 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_22[] = { 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D };
static const symbol s_23[] = { 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_24[] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x8D };
static const symbol s_25[] = { 0xE0, 0xAF, 0x88 };
static const symbol s_26[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_27[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8D };
static const symbol s_28[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8D };
static const symbol s_29[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_30[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_31[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_32[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x8D };
static const symbol s_33[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_34[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_35[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_36[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_37[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_38[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x81 };
static const symbol s_39[] = { 0xE0, 0xAE, 0x99, 0xE0, 0xAF, 0x8D };
static const symbol s_40[] = { 0xE0, 0xAF, 0x88 };
static const symbol s_41[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_42[] = { 0xE0, 0xAE, 0x99, 0xE0, 0xAF, 0x8D };
static const symbol s_43[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_44[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_45[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_46[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x99, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_47[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_48[] = { 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_49[] = { 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_50[] = { 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_51[] = { 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_52[] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_53[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_54[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_55[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_56[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_57[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x88 };
static const symbol s_58[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_59[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAE, 0xBF };
static const symbol s_60[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF };
static const symbol s_61[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xAF };
static const symbol s_62[] = { 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x81 };
static const symbol s_63[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB3 };
static const symbol s_64[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x88, 0xE0, 0xAE, 0xAF };
static const symbol s_65[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x88 };
static const symbol s_66[] = { 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_67[] = { 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB2 };
static const symbol s_68[] = { 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xA9 };
static const symbol s_69[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xBF };
static const symbol s_70[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_71[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x88 };
static const symbol s_72[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x88 };
static const symbol s_73[] = { 0xE0, 0xAF, 0x88 };
static const symbol s_74[] = { 0xE0, 0xAF, 0x88 };
static const symbol s_75[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_76[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_77[] = { 0xE0, 0xAF, 0x8A, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_78[] = { 0xE0, 0xAF, 0x8B, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81 };
static const symbol s_79[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_80[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D };
static const symbol s_81[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_82[] = { 0xE0, 0xAE, 0xAE };
static const symbol s_83[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x81 };
static const symbol s_84[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xA8, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81 };
static const symbol s_85[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0x9F };
static const symbol s_86[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0x9F, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_87[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_88[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x88 };
static const symbol s_89[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xAE, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_90[] = { 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_91[] = { 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_92[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_93[] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAE, 0xA3, 0xE0, 0xAF, 0x8D };
static const symbol s_94[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_95[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x87, 0xE0, 0xAE, 0xB2, 0xE0, 0xAF, 0x8D };
static const symbol s_96[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x87, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D };
static const symbol s_97[] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x80, 0xE0, 0xAE, 0xB4, 0xE0, 0xAF, 0x8D };
static const symbol s_98[] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_99[] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81 };
static const symbol s_100[] = { 0xE0, 0xAF, 0x80 };
static const symbol s_101[] = { 0xE0, 0xAE, 0xBF };
static const symbol s_102[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_103[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_104[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_105[] = { 0xE0, 0xAE, 0xAE, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_106[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_107[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_108[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_109[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_110[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_111[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_112[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_113[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_114[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_115[] = { 0xE0, 0xAE, 0xA9 };
static const symbol s_116[] = { 0xE0, 0xAE, 0xAA };
static const symbol s_117[] = { 0xE0, 0xAE, 0x95 };
static const symbol s_118[] = { 0xE0, 0xAE, 0xA4 };
static const symbol s_119[] = { 0xE0, 0xAE, 0xAF };
static const symbol s_120[] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_121[] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_122[] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_123[] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81 };
static const symbol s_124[] = { 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x8D, 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x81 };
static const symbol s_125[] = { 0xE0, 0xAE, 0xAA, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_126[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_127[] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_128[] = { 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_129[] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_130[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_131[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x88 };
static const symbol s_132[] = { 0xE0, 0xAE, 0xB5, 0xE0, 0xAF, 0x88 };
static const symbol s_133[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_134[] = { 0xE0, 0xAE, 0x9A };
static const symbol s_135[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xB3, 0xE0, 0xAF, 0x8D };
static const symbol s_136[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_137[] = { 0xE0, 0xAF, 0x87, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_138[] = { 0xE0, 0xAE, 0xBE };
static const symbol s_139[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_140[] = { 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_141[] = { 0xE0, 0xAF, 0x87, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_142[] = { 0xE0, 0xAF, 0x8B, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_143[] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_144[] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_145[] = { 0xE0, 0xAE, 0x9F, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_146[] = { 0xE0, 0xAE, 0xB1, 0xE0, 0xAF, 0x81, 0xE0, 0xAE, 0xAE, 0xE0, 0xAF, 0x8D };
static const symbol s_147[] = { 0xE0, 0xAE, 0xBE, 0xE0, 0xAE, 0xAF, 0xE0, 0xAF, 0x8D };
static const symbol s_148[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x86, 0xE0, 0xAE, 0xA9, 0xE0, 0xAF, 0x8D };
static const symbol s_149[] = { 0xE0, 0xAE, 0xA9, 0xE0, 0xAE, 0xBF, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_150[] = { 0xE0, 0xAF, 0x80, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_151[] = { 0xE0, 0xAF, 0x80, 0xE0, 0xAE, 0xAF, 0xE0, 0xAE, 0xB0, 0xE0, 0xAF, 0x8D };
static const symbol s_152[] = { 0xE0, 0xAF, 0x8D };
static const symbol s_153[] = { 0xE0, 0xAE, 0x95, 0xE0, 0xAF, 0x81 };
static const symbol s_154[] = { 0xE0, 0xAE, 0xA4, 0xE0, 0xAF, 0x81 };
static const symbol s_155[] = { 0xE0, 0xAF, 0x8D };

static int r_has_min_length(struct SN_env * z) { /* forwardmode */
    z->I[0] = len_utf8(z->p); /* $length = <integer expression>, line 105 */
    if (!(z->I[0] > 4)) return 0; /* $length > <integer expression>, line 106 */
    return 1;
}

static int r_fix_va_start(struct SN_env * z) { /* forwardmode */
    {   int c1 = z->c; /* or, line 110 */
        {   int c2 = z->c; /* and, line 110 */
            {   int c3 = z->c; /* try, line 110 */
                if (!(eq_s(z, 6, s_0))) { z->c = c3; goto lab2; } /* literal, line 110 */
            lab2:
                ;
            }
            z->c = c2;
            z->bra = z->c; /* [, line 110 */
        }
        if (!(eq_s(z, 6, s_1))) goto lab1; /* literal, line 110 */
        z->ket = z->c; /* ], line 110 */
        {   int ret = slice_from_s(z, 3, s_2); /* <-, line 110 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab1:
        z->c = c1;
        {   int c4 = z->c; /* and, line 111 */
            {   int c5 = z->c; /* try, line 111 */
                if (!(eq_s(z, 6, s_3))) { z->c = c5; goto lab4; } /* literal, line 111 */
            lab4:
                ;
            }
            z->c = c4;
            z->bra = z->c; /* [, line 111 */
        }
        if (!(eq_s(z, 6, s_4))) goto lab3; /* literal, line 111 */
        z->ket = z->c; /* ], line 111 */
        {   int ret = slice_from_s(z, 3, s_5); /* <-, line 111 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab3:
        z->c = c1;
        {   int c6 = z->c; /* and, line 112 */
            {   int c7 = z->c; /* try, line 112 */
                if (!(eq_s(z, 6, s_6))) { z->c = c7; goto lab6; } /* literal, line 112 */
            lab6:
                ;
            }
            z->c = c6;
            z->bra = z->c; /* [, line 112 */
        }
        if (!(eq_s(z, 6, s_7))) goto lab5; /* literal, line 112 */
        z->ket = z->c; /* ], line 112 */
        {   int ret = slice_from_s(z, 3, s_8); /* <-, line 112 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab5:
        z->c = c1;
        {   int c8 = z->c; /* and, line 113 */
            {   int c9 = z->c; /* try, line 113 */
                if (!(eq_s(z, 6, s_9))) { z->c = c9; goto lab7; } /* literal, line 113 */
            lab7:
                ;
            }
            z->c = c8;
            z->bra = z->c; /* [, line 113 */
        }
        if (!(eq_s(z, 6, s_10))) return 0; /* literal, line 113 */
        z->ket = z->c; /* ], line 113 */
        {   int ret = slice_from_s(z, 3, s_11); /* <-, line 113 */
            if (ret < 0) return ret;
        }
    }
lab0:
    return 1;
}

static int r_fix_endings(struct SN_env * z) { /* forwardmode */
    z->B[2] = 1; /* set found_wrong_ending, line 117 */
    while(1) { /* repeat, line 118 */
        int c1 = z->c;
        if (!(z->B[2])) goto lab0; /* Boolean test found_wrong_ending, line 118 */
        {   int c2 = z->c; /* do, line 118 */
            {   int ret = r_fix_ending(z); /* call fix_ending, line 118 */
                if (ret == 0) goto lab1;
                if (ret < 0) return ret;
            }
        lab1:
            z->c = c2;
        }
        continue;
    lab0:
        z->c = c1;
        break;
    }
    return 1;
}

static int r_remove_question_prefixes(struct SN_env * z) { /* forwardmode */
    z->bra = z->c; /* [, line 122 */
    if (!(eq_s(z, 3, s_12))) return 0; /* literal, line 122 */
    if (!(find_among(z, a_0, 10))) return 0; /* among, line 122 */
    if (!(eq_s(z, 3, s_13))) return 0; /* literal, line 122 */
    z->ket = z->c; /* ], line 122 */
    {   int ret = slice_del(z); /* delete, line 122 */
        if (ret < 0) return ret;
    }
    {   int c1 = z->c; /* do, line 123 */
        {   int ret = r_fix_va_start(z); /* call fix_va_start, line 123 */
            if (ret == 0) goto lab0;
            if (ret < 0) return ret;
        }
    lab0:
        z->c = c1;
    }
    return 1;
}

static int r_fix_ending(struct SN_env * z) { /* forwardmode */
    z->B[2] = 0; /* unset found_wrong_ending, line 127 */
    z->I[0] = len_utf8(z->p); /* $length = <integer expression>, line 128 */
    if (!(z->I[0] > 3)) return 0; /* $length > <integer expression>, line 129 */
    z->lb = z->c; z->c = z->l; /* backwards, line 130 */

    {   int m1 = z->l - z->c; (void)m1; /* or, line 132 */
        z->ket = z->c; /* [, line 131 */
        if (z->c - 5 <= z->lb || (z->p[z->c - 1] != 141 && z->p[z->c - 1] != 164)) goto lab1; /* among, line 131 */
        if (!(find_among_b(z, a_1, 3))) goto lab1;
        z->bra = z->c; /* ], line 131 */
        {   int ret = slice_del(z); /* delete, line 131 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab1:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 133 */
        if (!(eq_s_b(z, 6, s_14))) goto lab2; /* literal, line 133 */
        {   int m_test2 = z->l - z->c; /* test, line 133 */
            if (!(find_among_b(z, a_2, 3))) goto lab2; /* among, line 133 */
            z->c = z->l - m_test2;
        }
        z->bra = z->c; /* ], line 133 */
        {   int ret = slice_del(z); /* delete, line 133 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab2:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 135 */
        {   int m3 = z->l - z->c; (void)m3; /* or, line 135 */
            if (!(eq_s_b(z, 12, s_15))) goto lab5; /* literal, line 135 */
            goto lab4;
        lab5:
            z->c = z->l - m3;
            if (!(eq_s_b(z, 12, s_16))) goto lab3; /* literal, line 135 */
        }
    lab4:
        z->bra = z->c; /* ], line 135 */
        {   int ret = slice_from_s(z, 6, s_17); /* <-, line 135 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab3:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 137 */
        if (!(eq_s_b(z, 12, s_18))) goto lab6; /* literal, line 137 */
        z->bra = z->c; /* ], line 137 */
        {   int ret = slice_from_s(z, 6, s_19); /* <-, line 137 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab6:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 140 */
        if (!(eq_s_b(z, 12, s_20))) goto lab7; /* literal, line 140 */
        z->bra = z->c; /* ], line 140 */
        {   int ret = slice_from_s(z, 6, s_21); /* <-, line 140 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab7:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 142 */
        if (!(eq_s_b(z, 12, s_22))) goto lab8; /* literal, line 142 */
        z->bra = z->c; /* ], line 142 */
        {   int ret = slice_from_s(z, 6, s_23); /* <-, line 142 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab8:
        z->c = z->l - m1;
        if (!(z->B[1])) goto lab9; /* Boolean test found_vetrumai_urupu, line 144 */
        z->ket = z->c; /* [, line 144 */
        if (!(eq_s_b(z, 12, s_24))) goto lab9; /* literal, line 144 */
        {   int m_test4 = z->l - z->c; /* test, line 144 */
            {   int m5 = z->l - z->c; (void)m5; /* not, line 144 */
                if (!(eq_s_b(z, 3, s_25))) goto lab10; /* literal, line 144 */
                goto lab9;
            lab10:
                z->c = z->l - m5;
            }
            z->c = z->l - m_test4;
        }
        z->bra = z->c; /* ], line 144 */
        {   int ret = slice_from_s(z, 6, s_26); /* <-, line 144 */
            if (ret < 0) return ret;
        }
        z->bra = z->c; /* ], line 144 */
        goto lab0;
    lab9:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 146 */
        {   int m6 = z->l - z->c; (void)m6; /* or, line 146 */
            if (!(eq_s_b(z, 9, s_27))) goto lab13; /* literal, line 146 */
            goto lab12;
        lab13:
            z->c = z->l - m6;
            if (!(eq_s_b(z, 15, s_28))) goto lab11; /* literal, line 146 */
        }
    lab12:
        z->bra = z->c; /* ], line 146 */
        {   int ret = slice_from_s(z, 3, s_29); /* <-, line 146 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab11:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 148 */
        if (!(eq_s_b(z, 3, s_30))) goto lab14; /* literal, line 148 */
        if (!(find_among_b(z, a_3, 6))) goto lab14; /* among, line 148 */
        if (!(eq_s_b(z, 3, s_31))) goto lab14; /* literal, line 148 */
        if (!(find_among_b(z, a_4, 6))) goto lab14; /* among, line 148 */
        z->bra = z->c; /* ], line 148 */
        {   int ret = slice_del(z); /* delete, line 148 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab14:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 150 */
        if (!(eq_s_b(z, 9, s_32))) goto lab15; /* literal, line 150 */
        z->bra = z->c; /* ], line 150 */
        {   int ret = slice_from_s(z, 3, s_33); /* <-, line 150 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab15:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 152 */
        if (!(eq_s_b(z, 3, s_34))) goto lab16; /* literal, line 152 */
        if (!(find_among_b(z, a_5, 6))) goto lab16; /* among, line 152 */
        z->bra = z->c; /* ], line 152 */
        {   int ret = slice_del(z); /* delete, line 152 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab16:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 154 */
        if (!(eq_s_b(z, 3, s_35))) goto lab17; /* literal, line 154 */
        {   int m7 = z->l - z->c; (void)m7; /* or, line 154 */
            if (z->c - 2 <= z->lb || z->p[z->c - 1] >> 5 != 5 || !((4030464 >> (z->p[z->c - 1] & 0x1f)) & 1)) goto lab19; /* among, line 154 */
            if (!(find_among_b(z, a_6, 6))) goto lab19;
            goto lab18;
        lab19:
            z->c = z->l - m7;
            if (!(find_among_b(z, a_7, 6))) goto lab17; /* among, line 154 */
        }
    lab18:
        if (!(eq_s_b(z, 3, s_36))) goto lab17; /* literal, line 154 */
        z->bra = z->c; /* ], line 154 */
        {   int ret = slice_from_s(z, 3, s_37); /* <-, line 154 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab17:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 156 */
        if (!(find_among_b(z, a_8, 3))) goto lab20; /* among, line 156 */
        z->bra = z->c; /* ], line 156 */
        {   int ret = slice_del(z); /* delete, line 156 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab20:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 158 */
        if (!(eq_s_b(z, 6, s_38))) goto lab21; /* literal, line 158 */
        {   int m_test8 = z->l - z->c; /* test, line 158 */
            {   int m9 = z->l - z->c; (void)m9; /* not, line 158 */
                if (!(find_among_b(z, a_9, 8))) goto lab22; /* among, line 158 */
                goto lab21;
            lab22:
                z->c = z->l - m9;
            }
            z->c = z->l - m_test8;
        }
        z->bra = z->c; /* ], line 158 */
        {   int ret = slice_del(z); /* delete, line 158 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab21:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 160 */
        if (!(eq_s_b(z, 6, s_39))) goto lab23; /* literal, line 160 */
        {   int m_test10 = z->l - z->c; /* test, line 160 */
            {   int m11 = z->l - z->c; (void)m11; /* not, line 160 */
                if (!(eq_s_b(z, 3, s_40))) goto lab24; /* literal, line 160 */
                goto lab23;
            lab24:
                z->c = z->l - m11;
            }
            z->c = z->l - m_test10;
        }
        z->bra = z->c; /* ], line 160 */
        {   int ret = slice_from_s(z, 6, s_41); /* <-, line 160 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab23:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 162 */
        if (!(eq_s_b(z, 6, s_42))) goto lab25; /* literal, line 162 */
        z->bra = z->c; /* ], line 162 */
        {   int ret = slice_del(z); /* delete, line 162 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab25:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 164 */
        if (!(eq_s_b(z, 3, s_43))) return 0; /* literal, line 164 */
        {   int m_test12 = z->l - z->c; /* test, line 164 */
            {   int m13 = z->l - z->c; (void)m13; /* or, line 164 */
                if (!(find_among_b(z, a_10, 8))) goto lab27; /* among, line 164 */
                goto lab26;
            lab27:
                z->c = z->l - m13;
                if (!(eq_s_b(z, 3, s_44))) return 0; /* literal, line 164 */
            }
        lab26:
            z->c = z->l - m_test12;
        }
        z->bra = z->c; /* ], line 164 */
        {   int ret = slice_del(z); /* delete, line 164 */
            if (ret < 0) return ret;
        }
    }
lab0:
    z->c = z->lb;
    z->B[2] = 1; /* set found_wrong_ending, line 167 */
    return 1;
}

static int r_remove_pronoun_prefixes(struct SN_env * z) { /* forwardmode */
    z->B[0] = 0; /* unset found_a_match, line 171 */
    z->bra = z->c; /* [, line 172 */
    if (z->c + 2 >= z->l || z->p[z->c + 2] >> 5 != 4 || !((672 >> (z->p[z->c + 2] & 0x1f)) & 1)) return 0; /* among, line 172 */
    if (!(find_among(z, a_11, 3))) return 0;
    if (!(find_among(z, a_12, 10))) return 0; /* among, line 172 */
    if (!(eq_s(z, 3, s_45))) return 0; /* literal, line 172 */
    z->ket = z->c; /* ], line 172 */
    {   int ret = slice_del(z); /* delete, line 172 */
        if (ret < 0) return ret;
    }
    z->B[0] = 1; /* set found_a_match, line 173 */
    {   int c1 = z->c; /* do, line 174 */
        {   int ret = r_fix_va_start(z); /* call fix_va_start, line 174 */
            if (ret == 0) goto lab0;
            if (ret < 0) return ret;
        }
    lab0:
        z->c = c1;
    }
    return 1;
}

static int r_remove_plural_suffix(struct SN_env * z) { /* forwardmode */
    z->B[0] = 0; /* unset found_a_match, line 178 */
    z->lb = z->c; z->c = z->l; /* backwards, line 179 */

    {   int m1 = z->l - z->c; (void)m1; /* or, line 180 */
        z->ket = z->c; /* [, line 180 */
        if (!(eq_s_b(z, 18, s_46))) goto lab1; /* literal, line 180 */
        {   int m_test2 = z->l - z->c; /* test, line 180 */
            {   int m3 = z->l - z->c; (void)m3; /* not, line 180 */
                if (!(find_among_b(z, a_13, 6))) goto lab2; /* among, line 180 */
                goto lab1;
            lab2:
                z->c = z->l - m3;
            }
            z->c = z->l - m_test2;
        }
        z->bra = z->c; /* ], line 180 */
        {   int ret = slice_from_s(z, 3, s_47); /* <-, line 180 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab1:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 181 */
        if (!(eq_s_b(z, 15, s_48))) goto lab3; /* literal, line 181 */
        z->bra = z->c; /* ], line 181 */
        {   int ret = slice_from_s(z, 6, s_49); /* <-, line 181 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab3:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 182 */
        if (!(eq_s_b(z, 15, s_50))) goto lab4; /* literal, line 182 */
        z->bra = z->c; /* ], line 182 */
        {   int ret = slice_from_s(z, 6, s_51); /* <-, line 182 */
            if (ret < 0) return ret;
        }
        goto lab0;
    lab4:
        z->c = z->l - m1;
        z->ket = z->c; /* [, line 183 */
        if (!(eq_s_b(z, 9, s_52))) return 0; /* literal, line 183 */
        z->bra = z->c; /* ], line 183 */
        {   int ret = slice_del(z); /* delete, line 183 */
            if (ret < 0) return ret;
        }
    }
lab0:
    z->B[0] = 1; /* set found_a_match, line 184 */
    z->c = z->lb;
    return 1;
}

static int r_remove_question_suffixes(struct SN_env * z) { /* forwardmode */
    {   int ret = r_has_min_length(z); /* call has_min_length, line 189 */
        if (ret <= 0) return ret;
    }
    z->B[0] = 0; /* unset found_a_match, line 190 */
    z->lb = z->c; z->c = z->l; /* backwards, line 191 */

    {   int m1 = z->l - z->c; (void)m1; /* do, line 192 */
        z->ket = z->c; /* [, line 193 */
        if (!(find_among_b(z, a_14, 3))) goto lab0; /* among, line 193 */
        z->bra = z->c; /* ], line 193 */
        {   int ret = slice_from_s(z, 3, s_53); /* <-, line 193 */
            if (ret < 0) return ret;
        }
        z->B[0] = 1; /* set found_a_match, line 194 */
    lab0:
        z->c = z->l - m1;
    }
    z->c = z->lb;
    {   int c2 = z->c; /* do, line 197 */
        {   int ret = r_fix_endings(z); /* call fix_endings, line 197 */
            if (ret == 0) goto lab1;
            if (ret < 0) return ret;
        }
    lab1:
        z->c = c2;
    }
    return 1;
}

static int r_remove_command_suffixes(struct SN_env * z) { /* forwardmode */
    {   int ret = r_has_min_length(z); /* call has_min_length, line 201 */
        if (ret <= 0) return ret;
    }
    z->B[0] = 0; /* unset found_a_match, line 202 */
    z->lb = z->c; z->c = z->l; /* backwards, line 203 */

    z->ket = z->c; /* [, line 204 */
    if (z->c - 5 <= z->lb || z->p[z->c - 1] != 191) return 0; /* among, line 204 */
    if (!(find_among_b(z, a_15, 2))) return 0;
    z->bra = z->c; /* ], line 204 */
    {   int ret = slice_del(z); /* delete, line 204 */
        if (ret < 0) return ret;
    }
    z->B[0] = 1; /* set found_a_match, line 205 */
    z->c = z->lb;
    return 1;
}

static int r_remove_um(struct SN_env * z) { /* forwardmode */
    z->B[0] = 0; /* unset found_a_match, line 210 */
    {   int ret = r_has_min_length(z); /* call has_min_length, line 211 */
        if (ret <= 0) return ret;
    }
    z->lb = z->c; z->c = z->l; /* backwards, line 212 */

    z->ket = z->c; /* [, line 212 */
    if (!(eq_s_b(z, 9, s_54))) return 0; /* literal, line 212 */
    z->bra = z->c; /* ], line 212 */
    {   int ret = slice_from_s(z, 3, s_55); /* <-, line 212 */
        if (ret < 0) return ret;
    }
    z->B[0] = 1; /* set found_a_match, line 213 */
    z->c = z->lb;
    {   int c1 = z->c; /* do, line 215 */
        {   int ret = r_fix_ending(z); /* call fix_ending, line 215 */
            if (ret == 0) goto lab0;
            if (ret < 0) return ret;
        }
    lab0:
        z->c = c1;
    }
    return 1;
}

static int r_remove_common_word_endings(struct SN_env * z) { /* forwardmode */
    z->B[0] = 0; /* unset found_a_match, line 222 */
    {   int ret = r_has_min_length(z); /* call has_min_length, line 223 */
        if (ret <= 0) return ret;
    }
    z->lb = z->c; z->c = z->l; /* backwards, line 224 */

    {   int m1 = z->l - z->c; (void)m1; /* or, line 241 */
        {   int m_test2 = z->l - z->c; /* test, line 225 */
            z->ket = z->c; /* [, line 225 */
            {   int m3 = z->l - z->c; (void)m3; /* or, line 225 */
                if (!(eq_s_b(z, 12, s_56))) goto lab3; /* literal, line 225 */
                goto lab2;
            lab3:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 15, s_57))) goto lab4; /* literal, line 226 */
                goto lab2;
            lab4:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 12, s_58))) goto lab5; /* literal, line 227 */
                goto lab2;
            lab5:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 15, s_59))) goto lab6; /* literal, line 228 */
                goto lab2;
            lab6:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 9, s_60))) goto lab7; /* literal, line 229 */
                goto lab2;
            lab7:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 12, s_61))) goto lab8; /* literal, line 230 */
                goto lab2;
            lab8:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 15, s_62))) goto lab9; /* literal, line 231 */
                goto lab2;
            lab9:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 12, s_63))) goto lab10; /* literal, line 232 */
                goto lab2;
            lab10:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 12, s_64))) goto lab11; /* literal, line 233 */
                goto lab2;
            lab11:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 9, s_65))) goto lab12; /* literal, line 234 */
                goto lab2;
            lab12:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 15, s_66))) goto lab13; /* literal, line 235 */
                goto lab2;
            lab13:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 9, s_67))) goto lab14; /* literal, line 236 */
                {   int m_test4 = z->l - z->c; /* test, line 236 */
                    {   int m5 = z->l - z->c; (void)m5; /* not, line 236 */
                        if (!(find_among_b(z, a_16, 8))) goto lab15; /* among, line 236 */
                        goto lab14;
                    lab15:
                        z->c = z->l - m5;
                    }
                    z->c = z->l - m_test4;
                }
                goto lab2;
            lab14:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 6, s_68))) goto lab16; /* literal, line 237 */
                goto lab2;
            lab16:
                z->c = z->l - m3;
                if (!(eq_s_b(z, 9, s_69))) goto lab1; /* literal, line 238 */
            }
        lab2:
            z->bra = z->c; /* ], line 238 */
            {   int ret = slice_from_s(z, 3, s_70); /* <-, line 238 */
                if (ret < 0) return ret;
            }
            z->B[0] = 1; /* set found_a_match, line 239 */
            z->c = z->l - m_test2;
        }
        goto lab0;
    lab1:
        z->c = z->l - m1;
        {   int m_test6 = z->l - z->c; /* test, line 242 */
            z->ket = z->c; /* [, line 242 */
            if (!(find_among_b(z, a_17, 13))) return 0; /* among, line 242 */
            z->bra = z->c; /* ], line 255 */
            {   int ret = slice_del(z); /* delete, line 255 */
                if (ret < 0) return ret;
            }
            z->B[0] = 1; /* set found_a_match, line 256 */
            z->c = z->l - m_test6;
        }
    }
lab0:
    z->c = z->lb;
    {   int c7 = z->c; /* do, line 259 */
        {   int ret = r_fix_endings(z); /* call fix_endings, line 259 */
            if (ret == 0) goto lab17;
            if (ret < 0) return ret;
        }
    lab17:
        z->c = c7;
    }
    return 1;
}

static int r_remove_vetrumai_urupukal(struct SN_env * z) { /* forwardmode */
    z->B[0] = 0; /* unset found_a_match, line 263 */
    z->B[1] = 0; /* unset found_vetrumai_urupu, line 264 */
    {   int ret = r_has_min_length(z); /* call has_min_length, line 265 */
        if (ret <= 0) return ret;
    }
    z->lb = z->c; z->c = z->l; /* backwards, line 266 */

    {   int m1 = z->l - z->c; (void)m1; /* or, line 269 */
        {   int m_test2 = z->l - z->c; /* test, line 268 */
            z->ket = z->c; /* [, line 268 */
            if (!(eq_s_b(z, 6, s_71))) goto lab1; /* literal, line 268 */
            z->bra = z->c; /* ], line 268 */
            {   int ret = slice_del(z); /* delete, line 268 */
                if (ret < 0) return ret;
            }
            z->c = z->l - m_test2;
        }
        goto lab0;
    lab1:
        z->c = z->l - m1;
        {   int m_test3 = z->l - z->c; /* test, line 270 */
            z->ket = z->c; /* [, line 270 */
            {   int m4 = z->l - z->c; (void)m4; /* or, line 271 */
                {   int m5 = z->l - z->c; (void)m5; /* or, line 270 */
                    if (!(eq_s_b(z, 9, s_72))) goto lab6; /* literal, line 270 */
                    goto lab5;
                lab6:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 3, s_73))) goto lab4; /* literal, line 271 */
                }
            lab5:
                {   int m_test6 = z->l - z->c; /* test, line 271 */
                    {   int m7 = z->l - z->c; (void)m7; /* not, line 271 */
                        if (!(find_among_b(z, a_18, 6))) goto lab7; /* among, line 271 */
                        goto lab4;
                    lab7:
                        z->c = z->l - m7;
                    }
                    z->c = z->l - m_test6;
                }
                goto lab3;
            lab4:
                z->c = z->l - m4;
                if (!(eq_s_b(z, 3, s_74))) goto lab2; /* literal, line 272 */
                {   int m_test8 = z->l - z->c; /* test, line 272 */
                    if (!(find_among_b(z, a_19, 6))) goto lab2; /* among, line 272 */
                    if (!(eq_s_b(z, 3, s_75))) goto lab2; /* literal, line 272 */
                    z->c = z->l - m_test8;
                }
            }
        lab3:
            z->bra = z->c; /* ], line 273 */
            {   int ret = slice_from_s(z, 3, s_76); /* <-, line 273 */
                if (ret < 0) return ret;
            }
            z->c = z->l - m_test3;
        }
        goto lab0;
    lab2:
        z->c = z->l - m1;
        {   int m_test9 = z->l - z->c; /* test, line 276 */
            z->ket = z->c; /* [, line 276 */
            {   int m10 = z->l - z->c; (void)m10; /* or, line 277 */
                if (!(eq_s_b(z, 9, s_77))) goto lab10; /* literal, line 277 */
                goto lab9;
            lab10:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_78))) goto lab11; /* literal, line 278 */
                goto lab9;
            lab11:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_79))) goto lab12; /* literal, line 279 */
                goto lab9;
            lab12:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_80))) goto lab13; /* literal, line 280 */
                goto lab9;
            lab13:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_81))) goto lab14; /* literal, line 281 */
                {   int m_test11 = z->l - z->c; /* test, line 281 */
                    {   int m12 = z->l - z->c; (void)m12; /* not, line 281 */
                        if (!(eq_s_b(z, 3, s_82))) goto lab15; /* literal, line 281 */
                        goto lab14;
                    lab15:
                        z->c = z->l - m12;
                    }
                    z->c = z->l - m_test11;
                }
                goto lab9;
            lab14:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 15, s_83))) goto lab16; /* literal, line 282 */
                goto lab9;
            lab16:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 21, s_84))) goto lab17; /* literal, line 283 */
                goto lab9;
            lab17:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_85))) goto lab18; /* literal, line 284 */
                goto lab9;
            lab18:
                z->c = z->l - m10;
                if (!(z->I[0] >= 7)) goto lab19; /* $length >= <integer expression>, line 285 */
                if (!(eq_s_b(z, 12, s_86))) goto lab19; /* literal, line 285 */
                goto lab9;
            lab19:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_87))) goto lab20; /* literal, line 286 */
                goto lab9;
            lab20:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_88))) goto lab21; /* literal, line 287 */
                goto lab9;
            lab21:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 12, s_89))) goto lab22; /* literal, line 288 */
                goto lab9;
            lab22:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 6, s_90))) goto lab23; /* literal, line 289 */
                {   int m_test13 = z->l - z->c; /* test, line 289 */
                    {   int m14 = z->l - z->c; (void)m14; /* not, line 289 */
                        if (!(find_among_b(z, a_20, 8))) goto lab24; /* among, line 289 */
                        goto lab23;
                    lab24:
                        z->c = z->l - m14;
                    }
                    z->c = z->l - m_test13;
                }
                goto lab9;
            lab23:
                z->c = z->l - m10;
                if (!(eq_s_b(z, 9, s_91))) goto lab8; /* literal, line 290 */
            }
        lab9:
            z->bra = z->c; /* ], line 291 */
            {   int ret = slice_from_s(z, 3, s_92); /* <-, line 291 */
                if (ret < 0) return ret;
            }
            z->c = z->l - m_test9;
        }
        goto lab0;
    lab8:
        z->c = z->l - m1;
        {   int m_test15 = z->l - z->c; /* test, line 294 */
            z->ket = z->c; /* [, line 294 */
            {   int m16 = z->l - z->c; (void)m16; /* or, line 295 */
                if (!(eq_s_b(z, 9, s_93))) goto lab27; /* literal, line 295 */
                goto lab26;
            lab27:
                z->c = z->l - m16;
                if (!(eq_s_b(z, 12, s_94))) goto lab28; /* literal, line 296 */
                goto lab26;
            lab28:
                z->c = z->l - m16;
                if (!(eq_s_b(z, 12, s_95))) goto lab29; /* literal, line 297 */
                goto lab26;
            lab29:
                z->c = z->l - m16;
                if (!(eq_s_b(z, 12, s_96))) goto lab30; /* literal, line 298 */
                goto lab26;
            lab30:
                z->c = z->l - m16;
                if (!(eq_s_b(z, 12, s_97))) goto lab31; /* literal, line 299 */
                goto lab26;
            lab31:
                z->c = z->l - m16;
                if (!(eq_s_b(z, 12, s_98))) goto lab32; /* literal, line 300 */
                goto lab26;
            lab32:
                z->c = z->l - m16;
                if (!(eq_s_b(z, 6, s_99))) goto lab25; /* literal, line 301 */
                {   int m_test17 = z->l - z->c; /* test, line 301 */
                    {   int m18 = z->l - z->c; (void)m18; /* not, line 301 */
                        if (!(find_among_b(z, a_21, 8))) goto lab33; /* among, line 301 */
                        goto lab25;
                    lab33:
                        z->c = z->l - m18;
                    }
                    z->c = z->l - m_test17;
                }
            }
        lab26:
            z->bra = z->c; /* ], line 302 */
            {   int ret = slice_del(z); /* delete, line 302 */
                if (ret < 0) return ret;
            }
            z->c = z->l - m_test15;
        }
        goto lab0;
    lab25:
        z->c = z->l - m1;
        {   int m_test19 = z->l - z->c; /* test, line 305 */
            z->ket = z->c; /* [, line 305 */
            if (!(eq_s_b(z, 3, s_100))) return 0; /* literal, line 305 */
            z->bra = z->c; /* ], line 305 */
            {   int ret = slice_from_s(z, 3, s_101); /* <-, line 305 */
                if (ret < 0) return ret;
            }
            z->c = z->l - m_test19;
        }
    }
lab0:
    z->B[0] = 1; /* set found_a_match, line 307 */
    z->B[1] = 1; /* set found_vetrumai_urupu, line 308 */
    {   int m20 = z->l - z->c; (void)m20; /* do, line 309 */
        z->ket = z->c; /* [, line 309 */
        if (!(eq_s_b(z, 9, s_102))) goto lab34; /* literal, line 309 */
        z->bra = z->c; /* ], line 309 */
        {   int ret = slice_from_s(z, 3, s_103); /* <-, line 309 */
            if (ret < 0) return ret;
        }
    lab34:
        z->c = z->l - m20;
    }
    z->c = z->lb;
    {   int c21 = z->c; /* do, line 311 */
        {   int ret = r_fix_endings(z); /* call fix_endings, line 311 */
            if (ret == 0) goto lab35;
            if (ret < 0) return ret;
        }
    lab35:
        z->c = c21;
    }
    return 1;
}

static int r_remove_tense_suffixes(struct SN_env * z) { /* forwardmode */
    z->B[0] = 1; /* set found_a_match, line 315 */
    while(1) { /* repeat, line 316 */
        int c1 = z->c;
        if (!(z->B[0])) goto lab0; /* Boolean test found_a_match, line 316 */
        {   int c2 = z->c; /* do, line 316 */
            {   int ret = r_remove_tense_suffix(z); /* call remove_tense_suffix, line 316 */
                if (ret == 0) goto lab1;
                if (ret < 0) return ret;
            }
        lab1:
            z->c = c2;
        }
        continue;
    lab0:
        z->c = c1;
        break;
    }
    return 1;
}

static int r_remove_tense_suffix(struct SN_env * z) { /* forwardmode */
    z->B[0] = 0; /* unset found_a_match, line 320 */
    {   int ret = r_has_min_length(z); /* call has_min_length, line 321 */
        if (ret <= 0) return ret;
    }
    z->lb = z->c; z->c = z->l; /* backwards, line 322 */

    {   int m1 = z->l - z->c; (void)m1; /* do, line 323 */
        {   int m2 = z->l - z->c; (void)m2; /* or, line 330 */
            {   int m_test3 = z->l - z->c; /* test, line 324 */
                z->ket = z->c; /* [, line 324 */
                if (z->c - 8 <= z->lb || (z->p[z->c - 1] != 129 && z->p[z->c - 1] != 141)) goto lab2; /* among, line 324 */
                if (!(find_among_b(z, a_22, 2))) goto lab2;
                z->bra = z->c; /* ], line 327 */
                {   int ret = slice_del(z); /* delete, line 327 */
                    if (ret < 0) return ret;
                }
                z->B[0] = 1; /* set found_a_match, line 328 */
                z->c = z->l - m_test3;
            }
            goto lab1;
        lab2:
            z->c = z->l - m2;
            {   int m_test4 = z->l - z->c; /* test, line 331 */
                z->ket = z->c; /* [, line 331 */
                {   int m5 = z->l - z->c; (void)m5; /* or, line 332 */
                    if (!(eq_s_b(z, 12, s_104))) goto lab5; /* literal, line 332 */
                    goto lab4;
                lab5:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_105))) goto lab6; /* literal, line 333 */
                    goto lab4;
                lab6:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_106))) goto lab7; /* literal, line 334 */
                    goto lab4;
                lab7:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_107))) goto lab8; /* literal, line 335 */
                    goto lab4;
                lab8:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_108))) goto lab9; /* literal, line 336 */
                    goto lab4;
                lab9:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_109))) goto lab10; /* literal, line 337 */
                    goto lab4;
                lab10:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_110))) goto lab11; /* literal, line 338 */
                    {   int m_test6 = z->l - z->c; /* test, line 338 */
                        {   int m7 = z->l - z->c; (void)m7; /* not, line 338 */
                            if (z->c - 2 <= z->lb || z->p[z->c - 1] >> 5 != 4 || !((1951712 >> (z->p[z->c - 1] & 0x1f)) & 1)) goto lab12; /* among, line 338 */
                            if (!(find_among_b(z, a_23, 12))) goto lab12;
                            goto lab11;
                        lab12:
                            z->c = z->l - m7;
                        }
                        z->c = z->l - m_test6;
                    }
                    goto lab4;
                lab11:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_111))) goto lab13; /* literal, line 339 */
                    goto lab4;
                lab13:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_112))) goto lab14; /* literal, line 340 */
                    goto lab4;
                lab14:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_113))) goto lab15; /* literal, line 341 */
                    goto lab4;
                lab15:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_114))) goto lab16; /* literal, line 342 */
                    goto lab4;
                lab16:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 3, s_115))) goto lab17; /* literal, line 343 */
                    goto lab4;
                lab17:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 3, s_116))) goto lab18; /* literal, line 343 */
                    goto lab4;
                lab18:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 3, s_117))) goto lab19; /* literal, line 343 */
                    goto lab4;
                lab19:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 3, s_118))) goto lab20; /* literal, line 343 */
                    goto lab4;
                lab20:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 3, s_119))) goto lab21; /* literal, line 343 */
                    goto lab4;
                lab21:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_120))) goto lab22; /* literal, line 344 */
                    goto lab4;
                lab22:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_121))) goto lab23; /* literal, line 345 */
                    goto lab4;
                lab23:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_122))) goto lab24; /* literal, line 346 */
                    goto lab4;
                lab24:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 6, s_123))) goto lab25; /* literal, line 347 */
                    {   int m_test8 = z->l - z->c; /* test, line 347 */
                        {   int m9 = z->l - z->c; (void)m9; /* not, line 347 */
                            if (!(find_among_b(z, a_24, 8))) goto lab26; /* among, line 347 */
                            goto lab25;
                        lab26:
                            z->c = z->l - m9;
                        }
                        z->c = z->l - m_test8;
                    }
                    goto lab4;
                lab25:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 15, s_124))) goto lab27; /* literal, line 348 */
                    goto lab4;
                lab27:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_125))) goto lab28; /* literal, line 349 */
                    goto lab4;
                lab28:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 9, s_126))) goto lab29; /* literal, line 350 */
                    goto lab4;
                lab29:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_127))) goto lab30; /* literal, line 351 */
                    goto lab4;
                lab30:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_128))) goto lab31; /* literal, line 352 */
                    goto lab4;
                lab31:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_129))) goto lab32; /* literal, line 353 */
                    goto lab4;
                lab32:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 12, s_130))) goto lab33; /* literal, line 354 */
                    goto lab4;
                lab33:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 6, s_131))) goto lab34; /* literal, line 355 */
                    goto lab4;
                lab34:
                    z->c = z->l - m5;
                    if (!(eq_s_b(z, 6, s_132))) goto lab3; /* literal, line 356 */
                }
            lab4:
                z->bra = z->c; /* ], line 357 */
                {   int ret = slice_del(z); /* delete, line 357 */
                    if (ret < 0) return ret;
                }
                z->B[0] = 1; /* set found_a_match, line 358 */
                z->c = z->l - m_test4;
            }
            goto lab1;
        lab3:
            z->c = z->l - m2;
            {   int m_test10 = z->l - z->c; /* test, line 361 */
                z->ket = z->c; /* [, line 361 */
                {   int m11 = z->l - z->c; (void)m11; /* or, line 362 */
                    if (!(eq_s_b(z, 9, s_133))) goto lab37; /* literal, line 362 */
                    {   int m_test12 = z->l - z->c; /* test, line 362 */
                        {   int m13 = z->l - z->c; (void)m13; /* not, line 362 */
                            if (!(eq_s_b(z, 3, s_134))) goto lab38; /* literal, line 362 */
                            goto lab37;
                        lab38:
                            z->c = z->l - m13;
                        }
                        z->c = z->l - m_test12;
                    }
                    goto lab36;
                lab37:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_135))) goto lab39; /* literal, line 363 */
                    goto lab36;
                lab39:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_136))) goto lab40; /* literal, line 364 */
                    goto lab36;
                lab40:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_137))) goto lab41; /* literal, line 365 */
                    goto lab36;
                lab41:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 3, s_138))) goto lab42; /* literal, line 366 */
                    goto lab36;
                lab42:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_139))) goto lab43; /* literal, line 367 */
                    goto lab36;
                lab43:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_140))) goto lab44; /* literal, line 368 */
                    goto lab36;
                lab44:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_141))) goto lab45; /* literal, line 369 */
                    goto lab36;
                lab45:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_142))) goto lab46; /* literal, line 370 */
                    goto lab36;
                lab46:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_143))) goto lab47; /* literal, line 371 */
                    goto lab36;
                lab47:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_144))) goto lab48; /* literal, line 372 */
                    goto lab36;
                lab48:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_145))) goto lab49; /* literal, line 373 */
                    goto lab36;
                lab49:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_146))) goto lab50; /* literal, line 374 */
                    goto lab36;
                lab50:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_147))) goto lab51; /* literal, line 375 */
                    goto lab36;
                lab51:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_148))) goto lab52; /* literal, line 376 */
                    goto lab36;
                lab52:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_149))) goto lab53; /* literal, line 377 */
                    goto lab36;
                lab53:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 9, s_150))) goto lab54; /* literal, line 378 */
                    goto lab36;
                lab54:
                    z->c = z->l - m11;
                    if (!(eq_s_b(z, 12, s_151))) goto lab35; /* literal, line 379 */
                }
            lab36:
                z->bra = z->c; /* ], line 380 */
                {   int ret = slice_from_s(z, 3, s_152); /* <-, line 380 */
                    if (ret < 0) return ret;
                }
                z->B[0] = 1; /* set found_a_match, line 381 */
                z->c = z->l - m_test10;
            }
            goto lab1;
        lab35:
            z->c = z->l - m2;
            {   int m_test14 = z->l - z->c; /* test, line 384 */
                z->ket = z->c; /* [, line 384 */
                {   int m15 = z->l - z->c; (void)m15; /* or, line 384 */
                    if (!(eq_s_b(z, 6, s_153))) goto lab56; /* literal, line 384 */
                    goto lab55;
                lab56:
                    z->c = z->l - m15;
                    if (!(eq_s_b(z, 6, s_154))) goto lab0; /* literal, line 384 */
                }
            lab55:
                {   int m_test16 = z->l - z->c; /* test, line 384 */
                    if (!(eq_s_b(z, 3, s_155))) goto lab0; /* literal, line 384 */
                    z->c = z->l - m_test16;
                }
                z->bra = z->c; /* ], line 384 */
                {   int ret = slice_del(z); /* delete, line 384 */
                    if (ret < 0) return ret;
                }
                z->B[0] = 1; /* set found_a_match, line 385 */
                z->c = z->l - m_test14;
            }
        }
    lab1:
    lab0:
        z->c = z->l - m1;
    }
    {   int m17 = z->l - z->c; (void)m17; /* do, line 388 */
        z->ket = z->c; /* [, line 388 */
        if (z->c - 8 <= z->lb || (z->p[z->c - 1] != 141 && z->p[z->c - 1] != 177)) goto lab57; /* among, line 388 */
        if (!(find_among_b(z, a_25, 6))) goto lab57;
        z->bra = z->c; /* ], line 395 */
        {   int ret = slice_del(z); /* delete, line 395 */
            if (ret < 0) return ret;
        }
        z->B[0] = 1; /* set found_a_match, line 396 */
    lab57:
        z->c = z->l - m17;
    }
    z->c = z->lb;
    {   int c18 = z->c; /* do, line 399 */
        {   int ret = r_fix_endings(z); /* call fix_endings, line 399 */
            if (ret == 0) goto lab58;
            if (ret < 0) return ret;
        }
    lab58:
        z->c = c18;
    }
    return 1;
}

extern int tamil_UTF_8_stem(struct SN_env * z) { /* forwardmode */
    z->B[1] = 0; /* unset found_vetrumai_urupu, line 403 */
    {   int c1 = z->c; /* do, line 404 */
        {   int ret = r_fix_ending(z); /* call fix_ending, line 404 */
            if (ret == 0) goto lab0;
            if (ret < 0) return ret;
        }
    lab0:
        z->c = c1;
    }
    {   int ret = r_has_min_length(z); /* call has_min_length, line 405 */
        if (ret <= 0) return ret;
    }
    {   int c2 = z->c; /* do, line 406 */
        {   int ret = r_remove_question_prefixes(z); /* call remove_question_prefixes, line 406 */
            if (ret == 0) goto lab1;
            if (ret < 0) return ret;
        }
    lab1:
        z->c = c2;
    }
    {   int c3 = z->c; /* do, line 407 */
        {   int ret = r_remove_pronoun_prefixes(z); /* call remove_pronoun_prefixes, line 407 */
            if (ret == 0) goto lab2;
            if (ret < 0) return ret;
        }
    lab2:
        z->c = c3;
    }
    {   int c4 = z->c; /* do, line 408 */
        {   int ret = r_remove_question_suffixes(z); /* call remove_question_suffixes, line 408 */
            if (ret == 0) goto lab3;
            if (ret < 0) return ret;
        }
    lab3:
        z->c = c4;
    }
    {   int c5 = z->c; /* do, line 409 */
        {   int ret = r_remove_um(z); /* call remove_um, line 409 */
            if (ret == 0) goto lab4;
            if (ret < 0) return ret;
        }
    lab4:
        z->c = c5;
    }
    {   int c6 = z->c; /* do, line 410 */
        {   int ret = r_remove_common_word_endings(z); /* call remove_common_word_endings, line 410 */
            if (ret == 0) goto lab5;
            if (ret < 0) return ret;
        }
    lab5:
        z->c = c6;
    }
    {   int c7 = z->c; /* do, line 411 */
        {   int ret = r_remove_vetrumai_urupukal(z); /* call remove_vetrumai_urupukal, line 411 */
            if (ret == 0) goto lab6;
            if (ret < 0) return ret;
        }
    lab6:
        z->c = c7;
    }
    {   int c8 = z->c; /* do, line 412 */
        {   int ret = r_remove_plural_suffix(z); /* call remove_plural_suffix, line 412 */
            if (ret == 0) goto lab7;
            if (ret < 0) return ret;
        }
    lab7:
        z->c = c8;
    }
    {   int c9 = z->c; /* do, line 413 */
        {   int ret = r_remove_command_suffixes(z); /* call remove_command_suffixes, line 413 */
            if (ret == 0) goto lab8;
            if (ret < 0) return ret;
        }
    lab8:
        z->c = c9;
    }
    {   int c10 = z->c; /* do, line 414 */
        {   int ret = r_remove_tense_suffixes(z); /* call remove_tense_suffixes, line 414 */
            if (ret == 0) goto lab9;
            if (ret < 0) return ret;
        }
    lab9:
        z->c = c10;
    }
    return 1;
}

extern struct SN_env * tamil_UTF_8_create_env(void) { return SN_create_env(0, 1, 3); }

extern void tamil_UTF_8_close_env(struct SN_env * z) { SN_close_env(z, 0); }

