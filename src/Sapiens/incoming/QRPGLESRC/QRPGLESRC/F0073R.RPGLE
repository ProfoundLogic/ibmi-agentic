     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0073R - PDS ERROR MESSAGE RECORDER
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program writes PDS adjudication messages to FDMGP  **
     H**     (Bill Entry Message File). The following adjudications  **
     H**     are supported by this program:                          **
     H**        . State Adjudication                                 **
     H**        . Pre/Re-Certification                               **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED: 12/10/96         PROJECT: MC119                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F0070R - Bill Entry                                     **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*  12/15/97 C:56 Project: FS139                                **
     H*             1. Incorporate changes in FDMGP field names      **
     H*                after moved from FSCDRV library.              **
     H*                                                              **
     H*             2. Add processing for MDR and CRR error messages **
     H*                                                              **
     H*             3. Add XX as the first two bytes of MGMSG# for   **
     H*                errors from state adjudication                **
     H*                                                              **
     H*             4. Add table and logic to translate state errors **
     H*                that have character bytes to numeric.         **
     H*                   000AA --> 00101                            **
     H*                   000BB --> 00102                            **
     H*                   000CC --> 00103                            **
     H*                         .                                    **
     H*                         .                                    **
     H*                         .                                    **
     H*                   000ZZ --> 00126                            **
     H*                                                              **
     H*             5. Add table and logic to translate Pre-Cert     **
     H*                errors from 5 bytes to 7 bytes.               **
     H*                   PC001 --> PC00001                          **
     H*                   PC002 --> PC00002                          **
     H*                         .                                    **
     H*                         .                                    **
     H*                         .                                    **
     H*                   PC007 --> PC00007                          **
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      50       General usage.                                **
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**   FDMGP    (PMG01): Bill Entry Message File                 **
     H**   SMUPL    (PUP01): User Parameter File (USR)               **
     H*****************************************************************
     H*
     FSMUPL     IF   E           K DISK
     FFDMGP     O    E           K DISK

     D*
     D* ---------------------------------------------------------------
     D* Tables and arrays
     D* ---------------------------------------------------------------
     D*
     D SEQS            S              5    DIM(30) ASCEND                       SQ#S FRM ADJ PGM
     D ERIS            S              2    DIM(30)                              ER#S FRM ADJ PGM
     D*
     D SQPC            S              5    DIM(50) ASCEND                       SQ#S FRM PRE-CERT
     D ERPC            S              5    DIM(50)                              ER#S FRM PRE-CERT
     D*** BEGIN ADD *** 12/15/97 **************************************
     D SQMD            S              5    DIM(50) ASCEND                       SQ#S FRM MDR
     D ERMD            S              5    DIM(50)                              ER#S FRM MDR
     D*
     D SQCR            S              5    DIM(50) ASCEND                       SQ#S FRM CRR
     D ERCR            S              5    DIM(50)                              ER#S FRM CRR
     D*
     D** BEG CHG *** 11/19/98 ****************************************
     D******************* SER1    1 169  5   SER2    5 0 STATE ERRS W/ ALPHA
     D SER1            S              5    DIM(171) CTDATA PERRCD(1)            STATE ERRS W/ ALPHA
     D SER2            S              5  0 DIM(171) ALT(SER1)
     D** END CHG *** 11/19/98 ****************************************
     D PER1            S              5    DIM(7) CTDATA PERRCD(1)              PRECERT ERRORS
     D PER2            S              7    DIM(7) ALT(PER1)
     D*** END ADD *** 12/15/97 ****************************************
     D*
     D* ---------------------------------------------------------------
     D* External System Information Data Structure
     D* ---------------------------------------------------------------
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*----------------------------------------------------------------
     D* System date and time
     D*----------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      /EJECT

     C*----------------------------------------------------------------
     C*        Entry parm list
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQDCS#            5
     C                   PARM                    QQSEQS          150
     C                   PARM                    QQERIS           60
     C                   PARM                    QQSQPC          250
     C                   PARM                    QQERPC          250
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   PARM                    QQSQMD          250
     C                   PARM                    QQERMD          250
     C                   PARM                    QQSQCR          250
     C                   PARM                    QQERCR          250
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C*================================================================
     C*================================================================
     C*          M A I N L I N E
     C*================================================================
     C*================================================================
     C*
     C*          Execute SR00 for initialization
     C*          ------------------------------------------------------
     C                   EXSR      SR00
     C*
     C*          Loop through adjudication array
     C*          ------------------------------------------------------
     C*** BEGIN DEL *** 12/15/97 **************************************
     C**********           Z-ADD1         X       30
     C********** X         DOWLE30
     C********** ERIS,X    IFNE *ZEROS
     C**********           MOVE SEQS,X    MGSEQ#
     C**********           ADD  1         MGMSEQ
     C**********           MOVE ERIS,X    MGMSGP    P
     C*** END DEL *** 12/15/97 ****************************************
     C*
     C*
     C*** BEGIN ADD *** 12/15/97 **************************************
     C*
     C                   Z-ADD     1             X                 3 0
     C                   MOVE      '00000'       HLDSEQ            5
     C                   Z-ADD     0             MGMSEQ
     C     X             DOWLE     30
     C*
     C     ERIS(X)       IFNE      *ZEROS
     C     ERIS(X)       ANDNE     *BLANKS
     C*
     C                   MOVE      SEQS(X)       MGDSEQ
     C     SEQS(X)       IFEQ      HLDSEQ
     C                   ADD       1             MGMSEQ
     C                   ELSE
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      SEQS(X)       HLDSEQ
     C                   ENDIF
     C*
     C*
     C** TRANSLATE STATE ERROR IF CONTAINS ALPHA BYTES
     C*
     C                   Z-ADD     1             R                 3 0
     C                   MOVE      '00000'       ERHLD             5
     C                   MOVE      ERIS(X)       ERHLD
     C     ERHLD         LOOKUP    SER1(R)                                61
     C     *IN61         IFEQ      '1'
     C                   MOVE      SER2(R)       MGMSG#
     C                   ELSE
     C                   MOVE      ERHLD         MGMSG#
     C                   ENDIF                                                  IN61 IFEQ
     C*
     C                   MOVEL     'XX'          MGMSG#
     C*** END CHG *** 12/15/97 ****************************************
     C*
     C                   MOVE      'S'           MGSRC
     C                   WRITE     PMG01
     C                   ELSE                                                   ERIS /= blanks
     C                   ADD       30            X
     C                   END                                                    ERIS /= blanks
     C                   ADD       1             X
     C                   ENDDO                                                  X <= 30
     C*
     C*          Loop through Pre-Certification Array
     C*          ------------------------------------------------------
     C*** BEGIN DEL *** 12/15/97 **************************************
     C**********           Z-ADD1         X       30
     C********** X         DOWLE50
     C********** ERPC,X    IFNE *ZEROS
     C**********           MOVE SQPC,X    MGSEQ#
     C**********           ADD  1         MGMSEQ
     C**********           MOVE ERPC,X    MGMSGP    P
     C*** END DEL *** 12/15/97 ****************************************
     C*
     C*** BEGIN ADD *** 12/15/97 **************************************
     C*
     C                   Z-ADD     1             X
     C                   MOVE      '00000'       HLDSEQ
     C                   Z-ADD     0             MGMSEQ
     C     X             DOWLE     30
     C*
     C     ERPC(X)       IFNE      *ZEROS
     C     ERPC(X)       ANDNE     *BLANKS
     C*
     C                   MOVE      SQPC(X)       MGDSEQ
     C     SQPC(X)       IFEQ      HLDSEQ
     C                   ADD       1             MGMSEQ
     C                   ELSE
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      SQPC(X)       HLDSEQ
     C                   ENDIF
     C*
     C* TRANSLATE 5 BYTE PRE-CERT ERROR ID TO 7 BYTES
     C*
     C                   Z-ADD     1             R
     C                   MOVE      '00000'       PCHLD             5
     C                   MOVE      ERPC(X)       PCHLD
     C     PCHLD         LOOKUP    PER1(R)                                61
     C                   MOVEL(P)  PER2(R)       MGMSG#
     C*
     C*** END ADD *** 12/15/97 ****************************************
     C                   MOVE      'P'           MGSRC
     C                   WRITE     PMG01
     C                   ELSE                                                   ERPC /= blanks
     C                   ADD       50            X
     C                   END                                                    ERPC /= blanks
     C                   ADD       1             X
     C                   ENDDO                                                  X <= 50
     C*
     C*** BEGIN ADD *** 12/15/97 **************************************
     C*
     C*          Loop through MDR Array
     C*          ------------------------------------------------------
     C                   Z-ADD     1             X                 3 0
     C                   MOVE      '00000'       HLDSEQ
     C                   Z-ADD     0             MGMSEQ
     C     X             DOWLE     50
     C*
     C     ERMD(X)       IFNE      *ZEROS
     C     ERMD(X)       ANDNE     *BLANKS
     C*
     C                   MOVE      SQMD(X)       MGDSEQ
     C*
     C     SQMD(X)       IFEQ      HLDSEQ
     C                   ADD       1             MGMSEQ
     C                   ELSE
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      SQMD(X)       HLDSEQ
     C                   ENDIF
     C*
     C                   MOVE(P)   ERMD(X)       MGMSG#
     C                   MOVE      'P'           MGSRC
     C                   WRITE     PMG01
     C                   ELSE                                                   ERPC /= blanks
     C                   ADD       50            X
     C                   END                                                    ERPC /= blanks
     C                   ADD       1             X
     C                   ENDDO                                                  X <= 50
     C*
     C*          Loop through CRR Array
     C*          ------------------------------------------------------
     C                   Z-ADD     1             X
     C                   MOVE      '00000'       HLDSEQ
     C                   Z-ADD     0             MGMSEQ
     C     X             DOWLE     50
     C*
     C     ERCR(X)       IFNE      *ZEROS
     C     ERCR(X)       ANDNE     *BLANKS
     C*
     C                   MOVE      SQCR(X)       MGDSEQ
     C     SQCR(X)       IFEQ      HLDSEQ
     C                   ADD       1             MGMSEQ
     C                   ELSE
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      SQCR(X)       HLDSEQ
     C                   ENDIF
     C*
     C                   MOVE      ERCR(X)       MGMSG#
     C                   MOVE      'P'           MGSRC
     C                   WRITE     PMG01
     C                   ELSE                                                   ERPC /= blanks
     C                   ADD       50            X
     C                   END                                                    ERPC /= blanks
     C                   ADD       1             X
     C                   ENDDO                                                  X <= 50
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C*          End of program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
     C*
     C*================================================================
     C*================================================================
     C*          S U B R O U T I N E S
     C*==============14================================================
     C*================================================================
     C*  SR00    Initialization Subroutine
     C*================================================================
     C*
     C*          Get company # from User Parameter file
     C*          ------------------------------------------------------
     C     SR00          BEGSR
     C     Q1USER        CHAIN     PUP01                              50
     C   50              MOVE      *ZERO         UPCO#
     C                   Z-ADD     UPCO#         MGCO#
     C*
     C*          Get time
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C*
     C*          Move bill parameters to FDMGP fields
     C*          ------------------------------------------------------
     C                   MOVE      QQSUB         MGSUB#
     C                   MOVE      QQFND         MGFND
     C                   MOVE      QQFYR         MGFYR
     C                   MOVE      QQCASE        MGCASE
     C                   MOVE      QQDCS#        MGDCS#
     C*
     C*          Move array parameters to arrays
     C*          ------------------------------------------------------
     C                   MOVEA     QQSEQS        SEQS
     C                   MOVEA     QQERIS        ERIS
     C                   MOVEA     QQSQPC        SQPC
     C                   MOVEA     QQERPC        ERPC
     C*** BEGIN ADD *** 12/15/97 **************************************
     C                   MOVEA     QQSQMD        SQMD
     C                   MOVEA     QQERMD        ERMD
     C                   MOVEA     QQSQCR        SQCR
     C                   MOVEA     QQERCR        ERCR
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C*          Initialize FDMGP fields that do not change
     C*          ------------------------------------------------------
     C                   Z-ADD     0             MGEOB#
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           MOVE *BLANKS   MGMSGH
     C                   MOVE      *BLANKS       MGMSFX
     C*** END CHG *** 12/15/97 ****************************************
     C                   MOVE      *BLANKS       MGCSI
     C*** BEGIN DEL *** 12/15/97 **************************************
     C********** DSDATE    MULT 10000.01  MGTRDT
     C*** END DEL *** 12/15/97 ****************************************
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   Z-ADD     DSTIME        MGTIME
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           Z-ADDMGTRDT    MGDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*** END CHG *** 12/15/97 ****************************************
     C                   MOVE      Q1USER        MGIUSR
     C*** BEGIN CHG *** 12/15/97 **************************************
     C**********           Z-ADDMGTRDT    MGIDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGIDT
     C                   MOVEL     Q1PGM         MGIPGM
     C*** END CHG *** 12/15/97 ****************************************
     C                   Z-ADD     MGTIME        MGITIM
     C*
     C*          End of subroutine SR00
     C*          ------------------------------------------------------
     C     SR00X         ENDSR
     C*** BEGIN ADD *** 12/15/97 **************************************
**  SER1/SER2: STATE ERROR TRANSLATION TABLE FROM ALHA TO NUMERIC
000AA00100
000AB00101
000AC00102
000AD00103
000AE00104
000AF00105
000AG00106
000AH00107
000AI00108
000AJ00109
000AK00110
000AL00111
000AM00112
000AN00113
000AO00114
000AP00115
000AQ00116
000AR00117
000AS00118
000AT00119
000AU00120
000AV00121
000AW00122
000AX00123
000AY00124
000AZ00125
000BA00126
000BB00127
000BC00128
000BD00129
000BE00130
000BF00131
000BG00132
000BH00133
000BI00134
000BJ00135
000BK00136
000BL00137
000BM00138
000BN00139
000BO00140
000BP00141
000BQ00142
000BR00143
000BS00144
000BT00145
000BU00146
000BV00147
000BW00148
000BX00149
000BY00150
000BZ00151
000CA00152
000CB00153
000CC00154
000CD00155
000CE00156
000CF00157
000CG00158
000CH00159
000CI00160
000CJ00161
000CK00162
000CL00163
000CM00164
000CN00165
000CO00166
000CP00167
000CQ00168
000CR00169
000CS00170
000CT00171
000CU00172
000CV00173
000CW00174
000CX00175
000CY00176
000CZ00177
000DA00178
000DB00179
000DC00180
000DD00181
000DE00182
000DF00183
000DG00184
000DH00185
000DI00186
000DJ00187
000DK00188
000DL00189
000DM00190
000DN00191
000DO00192
000DP00193
000DQ00194
000DR00195
000DS00196
000DT00197
000DU00198
000DV00199
000DW00200
000DX00201
000DY00202
000DZ00203
000EA00204
000EB00205
000EC00206
000ED00207
000EE00208
000EF00209
000EG00210
000EH00211
000EI00212
000EJ00213
000EK00214
000EL00215
000EM00216
000EN00217
000EO00218
000EP00219
000EQ00220
000ER00221
000ES00222
000ET00223
000EU00224
000EV00225
000EW00226
000EX00227
000EY00228
000EZ00229
000FA00230
000FB00231
000FC00232
000FD00233
000FE00234
000FF00235
000FG00236
000FH00237
000FI00238
000FJ00239
000FK00240
000FL00241
000FM00242
000FN00243
000FO00244
000FP00245
000FQ00246
000FR00247
000FS00248
000FT00249
000FU00250
000FV00251
000FW00252
000FX00253
000FY00254
000FZ00255
000GA00256
000GB00257
000GC00258
000GD00255
000GE00260
000GF00261
000GG00262
000GH00263
000GI00264
000GJ00265
000GK00266
000GL00267
000GM00268
000GN00269
000GO00270
**  PER1/PER2: PRECERT ERROR TRANSLATION TABLE FROM LENGTH 5 TO 7
PC001PC00001
PC002PC00002
PC003PC00003
PC004PC00004
PC005PC00005
PC006PC00006
PC007PC00007
