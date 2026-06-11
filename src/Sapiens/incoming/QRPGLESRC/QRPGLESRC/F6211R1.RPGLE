     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F6211R1 - EDI 837 SPLIT FILE FEF1P to detail PFs by type
     H*****************************************************************
     H* Description:
     H*              - Read FEF1P;
     H*                   o  populate unique ID#; start value = 1
     H*                   o  Move data to output file according to rec-
     H*                      ord type in positions 1-2; write record
     H*              - For each 05 record found
     H*                   o  Increment unique ID
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 10/21/15 C:079 Support: MC025 37996                           *
     H*                Remap HCFA and UB ICD-10 codes                 *
     H*                                                               *
     H* 04/16/15 C:079 Project: MC395                                 *
     H*                ICD10 TX 837 EDI Changes                       *
     H*                                                               *
     H* 03/07/07 C:62 PROJECT MC283                                   *
     H*               UPDATE LIN# FOR FE51P, FE52P, FE53P, FE55P,     *
     H*                               FE57P AND FE60P                 *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* LR - Program is ended with all files closed.
     H*****************************************************************
     H*
     FFEF1P     IPE  E             DISK
     FFEHDP     O    E             DISK
     FFE05P     O    E             DISK
     FFE06P     O    E             DISK
     FFE07P     O    E             DISK
     FFE08P     O    E             DISK
     FFE10P     O    E             DISK
     FFE30P     O    E             DISK
     FFE40P     O    E             DISK
     FFE41P     O    E             DISK
     FFE42P     O    E             DISK
     FFE44P     O    E             DISK
     FFE50P     O    E             DISK
     FFE51P     O    E             DISK
     FFE52P     O    E             DISK
     FFE53P     O    E             DISK
     FFE55P     O    E             DISK
     FFE57P     O    E             DISK
     FFE60P     O    E             DISK
     FFETRP     O    E             DISK

     D*
     D*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     D*     Input data structure FEF1P record
     D F1DATA          DS                  INZ
     D*     Define record type
     D  FTYPE                  1      2
     D*     Define data to move to output
     D  FEA                    1    256
     D  FEB                  257    512
     D*** Beg Chg *** 04/16/15 ***************************************
     D*****  FEC                  513    713
     D  FEC                  513    733
     D*** End Chg *** 04/16/15 ***************************************
     D*
     D*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     D*     Intermediate data structure -- to append Unique ID #
     D                 DS                  INZ
     D*
     D*     Data from input
     D  DSA                   12    267
     D  DSB                  268    523
     D*** Beg Chg *** 04/16/15 ***************************************
     D***  DSC                  524    724
     D  DSC                  524    733
     D*** End Chg *** 04/16/15 ***************************************
     D*
     D*     Intermediate data to move to output
     D  IEA                    1    256
     D  IEB                  257    512
     D*** Beg Chg *** 04/16/15 ***************************************
     D***  IEC                  513    724
     D*** Beg Chg *** 10/21/15 ***************************************
     D***  IEC                  513    733
     D  IEC                  513    741
     D*** End Chg *** 10/21/15 ***************************************
     D*** End Chg *** 04/16/15 ***************************************
     D*
     D*     Unique ID #
     D  ID#                    1     11  0 INZ
     D*
     D*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     D*     Output data structures for the output files
     D DDHD          E DS                  EXTNAME(FEHDP)
     D DD05          E DS                  EXTNAME(FE05P)
     D DD06          E DS                  EXTNAME(FE06P)
     D DD07          E DS                  EXTNAME(FE07P)
     D DD08          E DS                  EXTNAME(FE08P)
     D DD10          E DS                  EXTNAME(FE10P)
     D DD30          E DS                  EXTNAME(FE30P)
     D  DD30B                257    403
     D DD40          E DS                  EXTNAME(FE40P)
     D  DD40B                257    512
     D*** Beg Chg *** 04/16/15 ***************************************
     D***  DD40C                513    582
     D*** Beg Chg *** 10/21/15 ***************************************
     D****  DD40C                513    733
     D  DD40C                513    741
     D*** End Chg *** 10/21/15 ***************************************
     D*** End Chg *** 04/16/15 ***************************************
     D DD41          E DS                  EXTNAME(FE41P)
     D DD42          E DS                  EXTNAME(FE42P)
     D  DD42B                257    435
     D DD44          E DS                  EXTNAME(FE44P)
     D DD50          E DS                  EXTNAME(FE50P)
     D DD51          E DS                  EXTNAME(FE51P)
     D DD52          E DS                  EXTNAME(FE52P)
     D DD53          E DS                  EXTNAME(FE53P)
     D DD55          E DS                  EXTNAME(FE55P)
     D DD57          E DS                  EXTNAME(FE57P)
     D DD60          E DS                  EXTNAME(FE60P)
     D  DD60B                257    512
     D  DD60C                513    697
     D DDTR          E DS                  EXTNAME(FETRP)

     I*===============================================================
     I*
     I*===============================================================
     I*    D A T A
     I*===============================================================
     IPF101         51
     I*

     C*===============================================================
     C*--------  MAINLINE -----------------------
     C*===============================================================
     C     *IN51         IFEQ      *ON
     C*
     C*
     C                   MOVEL(P)  FEA           DSA
     C                   SELECT
     C     FTYPE         WHENEQ    'HD'
     C                   MOVEL(P)  FEA           DDHD
     C                   WRITE     PHE01
     C*
     C     FTYPE         WHENEQ    '05'
     C                   ADD       1             ID#
     C                   MOVEL(P)  IEA           DD05
     C                   WRITE     PIN01
     C*
     C     FTYPE         WHENEQ    '06'
     C                   MOVEL(P)  IEA           DD06
     C                   WRITE     PAD01
     C*
     C     FTYPE         WHENEQ    '07'
     C                   MOVEL(P)  IEA           DD07
     C                   WRITE     PMC01
     C*
     C     FTYPE         WHENEQ    '08'
     C                   MOVEL(P)  IEA           DD08
     C                   WRITE     PEM01
     C*
     C     FTYPE         WHENEQ    '10'
     C                   MOVEL(P)  IEA           DD10
     C                   WRITE     PPR01
     C*
     C     FTYPE         WHENEQ    '30'
     C                   MOVEL(P)  IEA           DD30
     C                   MOVEL(P)  FEB           DSB
     C                   MOVEL(P)  IEB           DD30B
     C                   WRITE     PCL01
     C*
     C     FTYPE         WHENEQ    '40'
     C                   MOVEL(P)  IEA           DD40
     C                   MOVEL(P)  FEB           DSB
     C                   MOVEL(P)  IEB           DD40B
     C                   MOVEL(P)  FEC           DSC
     C                   MOVEL(P)  IEC           DD40C
     C                   WRITE     PBI01
     C*
     C     FTYPE         WHENEQ    '41'
     C                   MOVEL(P)  IEA           DD41
     C                   WRITE     PHO01
     C*
     C     FTYPE         WHENEQ    '42'
     C                   MOVEL(P)  IEA           DD42
     C                   MOVEL(P)  FEB           DSB
     C                   MOVEL(P)  IEB           DD42B
     C                   WRITE     PPH01
     C*
     C     FTYPE         WHENEQ    '44'
     C                   MOVEL(P)  IEA           DD44
     C                   WRITE     PTY01
     C*
     C     FTYPE         WHENEQ    '50'
     C                   MOVEL(P)  IEA           DD50
     C                   WRITE     PSE01
     C*
     C     FTYPE         WHENEQ    '51'
     C                   MOVEL(P)  IEA           DD51
     C*** BEGIN ADD *** 03/07/07 **************************************
     C                   MOVEL     SELIN#        PSLIN#
     C*** END   ADD *** 03/07/07 **************************************
     C                   WRITE     PPS01
     C*
     C     FTYPE         WHENEQ    '52'
     C                   MOVEL(P)  IEA           DD52
     C*** BEGIN ADD *** 03/07/07 **************************************
     C                   MOVEL     SELIN#        ISLIN#
     C*** END   ADD *** 03/07/07 **************************************
     C                   WRITE     PIS01
     C*
     C     FTYPE         WHENEQ    '53'
     C                   MOVEL(P)  IEA           DD53
     C*** BEGIN ADD *** 03/07/07 **************************************
     C                   MOVEL     SELIN#        RXLIN#
     C*** END   ADD *** 03/07/07 **************************************
     C                   WRITE     PRX01
     C*
     C     FTYPE         WHENEQ    '55'
     C                   MOVEL(P)  IEA           DD55
     C*** BEGIN ADD *** 03/07/07 **************************************
     C                   MOVEL     SELIN#        DELIN#
     C*** END   ADD *** 03/07/07 **************************************
     C                   WRITE     PDE01
     C*
     C     FTYPE         WHENEQ    '57'
     C                   MOVEL(P)  IEA           DD57
     C*** BEGIN ADD *** 03/07/07 **************************************
     C                   MOVEL     SELIN#        DMLIN#
     C*** END   ADD *** 03/07/07 **************************************
     C                   WRITE     PDM01
     C*
     C     FTYPE         WHENEQ    '60'
     C                   MOVEL(P)  IEA           DD60
     C                   MOVEL(P)  FEB           DSB
     C                   MOVEL(P)  IEB           DD60B
     C                   MOVEL(P)  FEC           DSC
     C                   MOVEL(P)  IEC           DD60C
     C*** BEGIN ADD *** 03/07/07 **************************************
     C                   MOVEL     SELIN#        COLIN#
     C*** END   ADD *** 03/07/07 **************************************
     C                   WRITE     PCO01
     C*
     C     FTYPE         WHENEQ    'TR'
     C                   MOVEL(P)  FEA           DDTR
     C                   WRITE     PTR01
     C*
     C                   ENDSL
     C*
     C*
     C                   ENDIF
