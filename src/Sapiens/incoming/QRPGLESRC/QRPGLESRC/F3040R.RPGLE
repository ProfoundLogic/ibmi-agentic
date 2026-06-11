     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F3040R - WORK WITH T/M ACTIVITY                C:62 04/20/98 *
     H*                                                               *
     H*  CALLED FROM: F0269R - WW MEDICAL CASE MANAGEMENT             *
     H*               F0276R - WW CASE MANAGEMENT SERVICES            *
     H*               F0350R - WW PR VIDER BILLS                      *
     H*               U0060R - WW PRE-CERTS                           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*               FA702R                                          *
     H*               F3040R01                                        *
     H*               F3040R02                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/06/06 C:62  PROJECT: 139MEM                               *
     H*                 ADD BILLING FLAG PROCESSING                   *
     H*                                                               *
     H*  05/02/05 C:62  PROJECT: MC111 46823                          *
     H*                 CORRECT CHAIN TO NDTMP                        *
     H*                                                               *
     H*  01/31/03 C:62  PROJECT: MC095 35599                          *
     H*                 CONDITION CALL OF FA724R WHEN SYS = M OR U.   *
     H*                                                               *
     H*  08/13/02 C:62  PROJECT: MCMNT                                *
     H*                 -COMPILE ONLY                                 *
     H*                                                               *
     H*  12/14/01 C:62  PROJECT: MC111 30098                          *
     H*                 -REDEFINE INVOICED EOBS TO ALSO BE IF         *
     H*                  EAEPFL = 'P'(BEEN THRU BATCH) IF AUTO CHECK  *
     H*                                                               *
     H*  09/01/01  C:62  PROJECT: MC111 28834                         *
     H*                  MAKE SURE FAEAP/FAEOP ARE DELETED IF NO T/M  *
     H*                                                               *
     H*  08/14/00  C:62  PROJECT: MC231                               *
     H*                  ADD F9=INFO                                  *
     H*                                                               *
     H*  05/19/00  C:62  PROJECT: ARMNT                               *
     H*                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.  *
     H*                                                               *
     H*  03/28/00  C:53  PROJECT: ARMNT                               *
     H*                  REPLACE DUPLICATE LOGICALS                   *
     H*                    NDTML14 WITH NDTML5                        *
     H*                    NDTML13 WITH NDTML4                        *
     H*                                                               *
     H*  03/13/00  C:62  PROJECT: ARMNT (BASE ONLY)                   *
     H*                  ADD CALL TO FA724R TO PGMEND.                *
     H*                                                               *
     H*  01/21/00  C:62  PROJECT: MC035 21507                         *
     H*                  ADD LOGICALS THAT REFLECT THE SUBSET.        *
     H*                                                               *
     H*  01/11/00  C:62  PROJECT: MC035 21035 (BASE ONLY)             *
     H*                  ALLOW ADD IF CALLED FROM REINVOICE FACILITY. *
     H*                                                               *
     H*  01/06/00  C:62  PROJECT: MC035 21256 (PBILLS ONLY)           *
     H*                  BLANK OUT EABBLF IF EABINV=0 AND EABAMT<>0.  *
     H*                                                               *
     H*  11/09/99  C:62  PROJECT: MC035 20609                         *
     H*                  NEED TO LOAD EOB# FOR PAGING AND POSITIONING.*
     H*                                                               *
     H*  10/25/99  C:62  PROJECT: MC216                               *
     H*                  ADD ABILITY TO HANDLE REHAB HOLD/RELEASE.    *
     H*                                                               *
     H*  10/07/99  C:62  PROJECT: MC212                               *
     H*                  IF CALLED FROM RE-INVOICE THEN ALLOW UPDATE. *
     H*                                                               *
     H*  09/29/99  C:62  PROJECT: MC212                               *
     H*                  IF CALLED FROM A/R ONLY SHOW THAT EOB.       *
     H*                                                               *
     H*  12/01/98  C:62  PROJECT: MC098 15287 AND 15313               *
     H*                  1) USE EOINV# NOT EOBLFL TO DET IF INVOICED. *
     H*                  2) UPDATE EOFLG5 DETERMINED BY T/M.          *
     H*                                                               *
     H*  08/19/98 C:62 PROJECT: MC177                                 *
     H*                VARIOUS CHANGES                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     H*  SPECIAL OPERATIONS:                                          *
     H*                                                               *
     H*    *IF CALLED FROM F0269R, F0350R, OR U0060R, @@RRN3 IS ZERO  *
     H*     AND ALL T/M'S DISPLAYED.                                  *
     H*    *IF CALLED FROM F0276R @@RRN3 HOLDS RRN OF SELECTED CASE   *
     H*     MGMT SVC CODE AND ONLY T/M'S OF THAT CASE MGMT SVC CODE   *
     H*     ARE DISPLAYED.                                            *
     H*                                                               *
     H*****************************************************************
     FF3040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FNDTMP     IF   E             DISK
     F                                     RENAME(PTM01:PFRCDF)
     F*** BEGIN CHG *** 11/09/99 **************************************
     F*
     F*  Primary logical file definition (for displaying items)
     F***********NDTML3  IF  E           K        DISK
     F**********                                    KINFDS DBFDS
     F**********  PTM01                             KRENAMEDBRCDF
     F*
     F*  Secondary logical file definition (for displaying items)
     F**********NDTML4  IF  E           K        DISK
     F**********                                    KINFDS DBFDS2
     F**********  PTM01                             KRENAMEDBRCDF2
     F*
     F*  Tertiary logical file definition (for displaying items)
     F**********NDTML5  IF  E           K        DISK
     F**********                                    KINFDS DBFDS3
     F**********  PTM01                             KRENAMEDBRCDF3
     F*
     F*  Primary logical file definition (for displaying items)
     FNDTML8    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PTM01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FNDTML9    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PTM01:DBRCDF2)
     F*
     F*  Tertiary logical file definition (for displaying items)
     FNDTML10   IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PTM01:DBRCDF3)
     F*** END   CHG *** 11/09/99 **************************************
     F*** BEGIN ADD *** 01/21/00 **************************************
     FNDTML12   IF   E           K DISK
     F                                     RENAME(PTM01:PTM02)
     F                                     INFDS(DBFDSA)
     F*** BEG CHG *** 03/28/00 *********************
     F***NDTML13 IF  E           K        DISK
     FNDTML4    IF   E           K DISK
     F*** END CHG *** 03/28/00 *********************
     F                                     RENAME(PTM01:PTM03)
     F                                     INFDS(DBFDSB)
     F*** BEG CHG *** 03/28/00 *********************
     F***NDTML14 IF  E           K        DISK
     FNDTML5    IF   E           K DISK
     F*** END CHG *** 03/28/00 *********************
     F                                     RENAME(PTM01:PTM04)
     F                                     INFDS(DBFDSC)
     FNDTML15   IF   E           K DISK
     F                                     RENAME(PTM01:PTM05)
     F                                     INFDS(DBFDSD)
     FNDTML16   IF   E           K DISK
     F                                     RENAME(PTM01:PTM06)
     F                                     INFDS(DBFDSE)
     FNDTML17   IF   E           K DISK
     F                                     RENAME(PTM01:PTM07)
     F                                     INFDS(DBFDSF)
     F*** END   ADD *** 01/21/00 **************************************
     F*** BEGIN CHG *** 12/01/98 **************************************
     F**********FAEOP   IF  E                    DISK
     FFAEOP     UF   E             DISK
     F*** END   CHG *** 12/01/98 **************************************
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEGIN ADD *** 08/13/98 **************************************
     F                                     INFDS(DBFDS5)
     FFAEOLL    IF   E           K DISK
     F                                     RENAME(PEO01:PEO03)
     F                                     INFDS(DBFDS4)
     F*** END   ADD *** 08/13/98 **************************************
     F*** BEGIN ADD *** 09/27/99 **************************************
     F*** BEGIN CHG *** 10/25/99 **************************************
     F**********NDTML1  IF  E           K        DISK
     FNDTML1    UF   E           K DISK
     F*** END   CHG *** 10/25/99 **************************************
     FFAEAL1    UF   E           K DISK
     F*** END   ADD *** 09/27/99 **************************************
     FFTGTL     IF   E           K DISK
     FFTMCP     IF   E             DISK
     FFTMCL     IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)
     FFAMSP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D*** BEGIN ADD *** 10/25/99 **************************************
     D HDR             S             78    DIM(6) CTDATA PERRCD(1)              Rehab  headings
     D*** END   ADD *** 10/25/99 **************************************
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(17) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(17) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*** BEGIN ADD *** 01/21/00 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*** END   ADD *** 01/21/00 **************************************
     D                 DS
     D  X1DAT                  1     25
     D  X1SVDT                 1     10
     D  X1MSCD                12     15
     D  X1MSEQ                17     21
     D  X1STAF                23     25
     D                 DS
     D  X2DAT                  1     25
     D  X2STAF                 1      3
     D  X2SVDT                 5     14
     D  X2MSCD                16     19
     D  X2MSEQ                21     25
     D                 DS
     D  X3DAT                  1     25
     D  X3MSCD                 1      4
     D  X3MSEQ                 6     10
     D  X3SVDT                12     21
     D  X3STAF                23     25
     D*** BEGIN ADD *** 10/25/99 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   ADD *** 10/25/99 **************************************
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Time/Materials Activ-
     D                                     ity')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F3040R01')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(7)
     D DL              C                   CONST('* * * DELETED * * *')
     D*** BEGIN ADD *** 08/13/98 **************************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')

     I*** END   ADD *** 08/13/98 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   PARM                    @@CERT           15
     C                   PARM                    @@EXT             3 0
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 09/29/99 **************************************
     C                   PARM                    @@EOB             1
     C*** END   ADD *** 09/29/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM                    @@RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    ##RRN3            9 0
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   PARM      @@CERT        ##CERT           15
     C                   PARM      @@EXT         ##EXT             3 0
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      @@RCN         ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C*  Change numeric field to character field.
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       #1RTCD            1
     C                   PARM      ##WCIN        #1WCIN            1
     C                   PARM      ##WCDK        #1WDCK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##KEY            40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN CHG *** 11/09/99 **************************************
     C**********           KFLD           KKEOB#
     C**********           KFLD           KKSVDT
     C                   KFLD                    KKSVDT
     C                   KFLD                    KKEOB#
     C*** END   CHG *** 11/09/99 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN CHG *** 11/09/99 **************************************
     C**********           KFLD           KKEOB#
     C**********           KFLD           KKSTAF
     C                   KFLD                    KKSTAF
     C                   KFLD                    KKEOB#
     C*** END   CHG *** 11/09/99 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN CHG *** 11/09/99 **************************************
     C**********           KFLD           KKEOB#
     C**********           KFLD           KKMSCD
     C**********           KFLD           KKMSEQ
     C                   KFLD                    KKMSCD
     C                   KFLD                    KKMSEQ
     C                   KFLD                    KKEOB#
     C*** END   CHG *** 11/09/99 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 01/21/00 **************************************
     C     KEY5          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C*
     C     KEY6          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C                   KFLD                    KKSVDT
     C                   KFLD                    KKSEQ#
     C*
     C     KEY7          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C                   KFLD                    KKSTAF
     C                   KFLD                    KKSEQ#
     C*
     C     KEY8          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C                   KFLD                    KKMSCD
     C                   KFLD                    KKMSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     KEY9          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C*
     C     KEY10         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C                   KFLD                    KKSVDT
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
     C*
     C     KEY11         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C                   KFLD                    KKSTAF
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
     C*
     C     KEY12         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C                   KFLD                    KKMSCD
     C                   KFLD                    KKMSEQ
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 01/21/00 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN CHG *** 05/19/00 **************************************
     C**********           KFLD           EORCN
     C                   KFLD                    @@RCN
     C*** END   CHG *** 05/19/00 **************************************
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*
     C     EOKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C*
     C     EOKY2         KLIST
     C                   KFLD                    TMCO#
     C                   KFLD                    TMSUB#
     C                   KFLD                    TMFND
     C                   KFLD                    TMFYR
     C                   KFLD                    TMCASE
     C                   KFLD                    TMEOB#
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     MCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           KFLD           KKEOB#
     C*** BEGIN ADD *** 09/27/99 **************************************
     C**********           KFLD           KKRCN
     C*** END   ADD *** 09/27/99 **************************************
     C                   KFLD                    K1EOB#
     C                   KFLD                    K1RCN
     C*** END   CHG *** 10/25/99 **************************************
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     TMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    TMCO#         KKCO#
     C     *LIKE         DEFINE    TMSUB#        KKSUB#
     C     *LIKE         DEFINE    TMFND         KKFND
     C     *LIKE         DEFINE    TMFYR         KKFYR
     C     *LIKE         DEFINE    TMCASE        KKCASE
     C     *LIKE         DEFINE    TMEOB#        KKEOB#
     C     *LIKE         DEFINE    TMSTAF        KKSTAF
     C     *LIKE         DEFINE    TMMSCD        KKMSCD
     C     *LIKE         DEFINE    TMMSEQ        KKMSEQ
     C     *LIKE         DEFINE    TMSVDT        KKSVDT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    TMSVDT        XXSVD1
     C     *LIKE         DEFINE    TMSVDT        XXSVD2
     C     *LIKE         DEFINE    MCKEY         KKKEY
     C     *LIKE         DEFINE    SSINVD        XXINVD
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     *LIKE         DEFINE    EOCERT        KKCERT
     C     *LIKE         DEFINE    EOEXT         KKEXT
     C     *LIKE         DEFINE    TMSEQ#        KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C     *LIKE         DEFINE    TMRCN         XXRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     *LIKE         DEFINE    TMRCN         KKRCN
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C     *LIKE         DEFINE    TMEOB#        K1EOB#
     C     *LIKE         DEFINE    TMRCN         K1RCN
     C*** END   CHG *** 10/25/99 **************************************
     C*** BEGIN ADD *** 01/21/00 **************************************
     C     *LIKE         DEFINE    TMINV#        KKINV#
     C*** END   ADD *** 01/21/00 **************************************
     C*** BEGIN ADD *** 01/31/03 **************************************
     C     *LIKE         DEFINE    EOSYS         HDSYS
     C*** END   ADD *** 01/31/02 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*** BEGIN ADD *** 01/21/00 **************************************
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRNA        SCDBRR
     C   22              Z-ADD     DBRRNB        SCDBRR
     C   23              Z-ADD     DBRRNC        SCDBRR
     C*
     C     SSINVD        WHENEQ    'N'
     C*
     C   21              Z-ADD     DBRRND        SCDBRR
     C   22              Z-ADD     DBRRNE        SCDBRR
     C   23              Z-ADD     DBRRNF        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END   ADD *** 11/02/98 **************************************
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSVD1        IFNE      *ZEROS
     C     SSSVD2        ORNE      *ZEROS
     C     SSMSCD        ORNE      *BLANKS
     C     SSINVD        ORNE      *BLANKS
     C     SSSYS         ORNE      *BLANKS
     C     SSEOB#        ORNE      *ZEROS
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     SSCERT        ORNE      *BLANKS
     C     SSEXT         ORNE      *ZEROS
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C     SSRCN         ORNE      *ZEROS
     C*** END   ADD *** 01/17/99 **************************************
     C*** BEGIN ADD *** 08/06/06 **************************************
     C     SSBFLG        ORNE      *BLANKS
     C*** END   ADD *** 08/06/06 **************************************
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C***  BEGIN ADD  ***  01/21/00  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  01/21/00  *********************************
     C*** BEGIN ADD *** 10/07/99 **************************************
     C*
     C     Q1PRMS        IFGT      8
     C*
     C     @@EOB         IFNE      'C'
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*
     C* KEEP FROM USING OPTION '2' IF THE TIME AND MATERIAL IS INVOICED
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '2'
     C*
     C                   Z-ADD     SCEOMB        KKEOB#
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   Z-ADD     SCRCN         KKRCN
     C*
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOINV#        IFNE      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C                   SETON                                        7190
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 09/27/99 **************************************
     C*** BEGIN CHG *** 12/01/98 **************************************
     C**********           Z-ADD@@RRN2    ##RRN2
     C**********           Z-ADDDBRRN5    ##RRN2
     C                   Z-ADD     SCRRN         ##RRN2
     C*** END   CHG *** 12/01/98 **************************************
     C*** END   CHG *** 09/27/99 **************************************
     C*
     C                   ENDIF                                                  EOINV# NE 0
     C*
     C* KEEP FROM USING OPTION '4' IF THE TIME AND MATERIAL IS INVOICED
     C*
     C     SCSEL         WHENEQ    '4'
     C*
     C                   Z-ADD     SCEOMB        KKEOB#
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   Z-ADD     SCRCN         KKRCN
     C*
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOINV#        IFNE      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   SETON                                        7290
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 09/27/99 **************************************
     C*** BEGIN CHG *** 12/01/98 **************************************
     C**********           Z-ADD@@RRN2    ##RRN2
     C**********           Z-ADDDBRRN5    ##RRN2
     C                   Z-ADD     SCRRN         ##RRN2
     C*** END   CHG *** 12/01/98 **************************************
     C*** END   CHG *** 09/27/99 **************************************
     C*
     C                   ENDIF                                                  EOINV# NE 0
     C*** BEGIN ADD *** 10/07/99 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* KEEP FROM USING OPTION '2' IF THE TIME AND MATERIAL IS INVOICED
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '2'
     C*
     C                   Z-ADD     SCEOMB        KKEOB#
     C                   Z-ADD     SCRCN         KKRCN
     C*
     C     ##PRVB        IFNE      'Y'
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*
     C     EOINV#        IFNE      *ZEROS
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   SETON                                        7190
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCRRN         ##RRN2
     C*
     C                   ENDIF                                                  EOINV# NE 0
     C*
     C* KEEP FROM USING OPTION '4' IF THE TIME AND MATERIAL IS INVOICED
     C*
     C     SCSEL         WHENEQ    '4'
     C*
     C                   Z-ADD     SCEOMB        KKEOB#
     C                   Z-ADD     SCRCN         KKRCN
     C*
     C     ##PRVB        IFNE      'Y'
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*
     C     EOINV#        IFNE      *ZEROS
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   SETON                                        7290
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCRRN         ##RRN2
     C*
     C                   ENDIF                                                  EOINV# NE 0
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C* IF OPTION '5' IS USED MAKE SURE THE CORRECT FAEOP RECORD
     C* IS PASSED TO F3040R01.
     C*
     C*** BEGIN CHG *** 10/07/99 **************************************
     C********** SCSEL     WHEQ '5'
     C     SCSEL         IFEQ      '5'
     C*** END   CHG *** 10/07/99 **************************************
     C*** BEGIN DEL *** 09/27/99 **************************************
     C*
     C**********           Z-ADDSCEOMB    KKEOB#
     C*
     C********** EOKY      CHAINPEO02                99
     C*** END   DEL *** 09/27/99 **************************************
     C*
     C*** BEGIN DEL *** 12/01/98 **************************************
     C********** EOINV#    IFNE *ZEROS
     C*** END   DEL *** 12/01/98 **************************************
     C*
     C*** BEGIN CHG *** 09/27/99 **************************************
     C**********           Z-ADDDBRRN5    ##RRN2
     C                   Z-ADD     SCRRN         ##RRN2
     C*** END   CHG *** 09/27/99 **************************************
     C*
     C*** BEGIN DEL *** 12/01/98 **************************************
     C**********           ELSE
     C*
     C**********           Z-ADD@@RRN2    ##RRN2
     C*
     C**********           ENDIF                           EOINV# NE 0
     C*** END   DEL *** 12/01/98 **************************************
     C*
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           ENDSL
     C                   ENDIF
     C*** END   CHG *** 10/07/99 **************************************
     C*
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     SCSEL         IFEQ      '21'
     C*
     C                   Z-ADD     SCEOMB        KKEOB#
     C                   Z-ADD     SCRCN         KKRCN
     C*
     C     ##PRVB        IFNE      'Y'
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*
     C     EOINV#        IFNE      *ZEROS
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   SETOFF                                       3132
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDIF
     C*
     C* THE LOWER LIMIT SERVICE DATE MUST BE VALID IF NOT EQUAL TO 0.
     C*
     C     *IN90         IFEQ      *OFF
     C     SSSVD1        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSSVD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSSVD1
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C* THE UPPER LIMIT SERVICE DATE MUST BE VALID IF NOT EQUAL TO 0.
     C*
     C     *IN90         IFEQ      *OFF
     C     SSSVD2        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSSVD2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSSVD2
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* SERVICE CODE MUST BE VALID IN NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SSMSCD        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SSMSCD        KKKEY
     C*
     C     MCKY          CHAIN     PMC02                              99
     C*
     C   99              SETON                                        7390
     C*
     C                   ENDIF
     C*
     C     SSSYS         IFEQ      'M'
     C*
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       18
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     SSEOB#        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSEOB#        KKEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   MOVEL     EOSYS         SCSYS
     C                   MOVEL     EOSYS         SSSYS
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       18
     C*
     C                   ENDIF
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 08/06/06 **************************************
     C     SSBFLG        IFNE      *BLANKS
     C     SSBFLG        ANDNE     'Y'
     C     SSBFLG        ANDNE     'N'
     C                   SETON                                        7490
     C                   ENDIF
     C*** END   ADD *** 08/06/06 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*** BEGIN CHG *** 08/13/98 **************************************
     C********** Q1PRMS    IFGT 5
     C     @@RRN3        IFNE      *ZEROS
     C*** END   CHG *** 08/13/98 **************************************
     C*
     C     @@RRN3        CHAIN     PMS01                              99
     C*
     C                   MOVEL     MSMSCD        SSMSCD
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   MOVEL     SCSYS         SSSYS
     C                   MOVEL     'N'           SSINVD
     C                   Z-ADD     EOEOB#        SSEOB#
     C                   MOVEL     MSSEQ#        SSSEQ#
     C*
     C                   SETON                                        17
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C*** BEGIN CHG *** 08/13/98 **************************************
     C**********           ENDIF
     C                   ELSE
     C*
     C                   SELECT
     C*
     C     SCSYS         WHENEQ    'M'
     C*
     C**********           MOVELSCSYS     SSSYS
     C*
     C*** BEGIN ADD *** 08/18/98 **************************************
     C     EOINV#        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDNE     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*** END   ADD *** 08/18/98 **************************************
     C*
     C                   MOVEL     'N'           SSINVD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'Y'           SSINVD
     C                   MOVEL     SCSYS         SSSYS
     C                   Z-ADD     EOEOB#        SSEOB#
     C*
     C                   ENDIF
     C*
     C**********           SETON                     18
     C*
     C     SCSYS         WHENEQ    'U'
     C*
     C                   MOVEL     SCSYS         SSSYS
     C*
     C     EOINV#        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDNE     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   MOVEL     'N'           SSINVD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'Y'           SSINVD
     C*
     C                   ENDIF
     C*
     C                   SETON                                        17
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C**********           Z-ADDEOEOB#    SSEOB#
     C*
     C     Q1PRMS        IFGT      6
     C*
     C                   MOVEL(P)  @@CERT        SSCERT
     C                   Z-ADD     @@EXT         SSEXT
     C*
     C     @@CERT        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  @@CERT        KKCERT
     C                   Z-ADD     @@EXT         KKEXT
     C*
     C     EOKY1         CHAIN     PEO03                              99
     C*
     C                   Z-ADD     EOEOB#        SSEOB#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SSEOB#        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  '*ALL'        SCEOB#
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SSEOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL(P)  ##NUM         SCEOB#
     C*
     C                   ENDIF
     C*
     C     SSEOB#        IFNE      *ZEROS
     C*
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       18
     C*
     C                   ENDIF
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C                   MOVE      EORCN         SSRCN
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C*
     C                   Z-ADD     EOEOB#        SSEOB#
     C                   SETON                                        18
     C*
     C                   MOVEL(P)  SSEOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL(P)  ##NUM         SCEOB#
     C*
     C                   ENDIF
     C*** END   ADD *** 01/17/99 **************************************
     C*** BEGIN ADD *** 09/29/99 **************************************
     C*
     C                   SETOFF                                       19
     C*
     C     Q1PRMS        IFGT      8
     C     @@EOB         ANDEQ     'Y'
     C*
     C                   SETON                                        1819
     C                   Z-ADD     EOEOB#        SSEOB#
     C*
     C                   MOVEL(P)  SSEOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL(P)  ##NUM         SCEOB#
     C*
     C                   MOVE      EORCN         SSRCN
     C*
     C     EABINV        IFNE      *ZEROS
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   MOVEL     'Y'           SSINVD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'N'           SSINVD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/29/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   MOVE      @@RCN         SSRCN
     C*** END   ADD *** 05/19/00 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*** BEGIN ADD *** 09/27/99 **************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C*** BEGIN ADD *** 12/01/98 **************************************
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   Z-ADD     EOEOB#        K1EOB#
     C*** BEGIN CHG *** 05/19/00 **************************************
     C**********           Z-ADDEORCN     K1RCN
     C                   Z-ADD     @@RCN         K1RCN
     C*** END   CHG *** 05/19/00 **************************************
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C*** BEGIN CHG *** 10/25/99 **************************************
     C********** TMKY      CHAINPTM01                99
     C     TMKY          CHAIN(N)  PTM01                              99
     C*** END   CHG *** 10/25/99 **************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'Y'           EOFLG5
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ' '           EOFLG5
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ELSE
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C  N99EAKY          CHAIN     PEA01                              99
     C*** BEGIN ADD *** 01/31/03 **************************************
     C                   MOVEL     EOSYS         HDSYS
     C*** END   ADD *** 01/31/03 **************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EACO#         KKCO#
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   Z-ADD     EAEOB#        K1EOB#
     C*** BEGIN CHG *** 05/19/00 **************************************
     C**********           Z-ADDEARCN     K1RCN
     C                   Z-ADD     @@RCN         K1RCN
     C*** END   CHG *** 05/19/00 **************************************
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C*** BEGIN CHG *** 10/25/99 **************************************
     C********** TMKY      CHAINPTM01                99
     C     TMKY          CHAIN(N)  PTM01                              99
     C*** END   CHG *** 10/25/99 **************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'Y'           EATMFL
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ' '           EATMFL
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C*** BEGIN DEL *** 12/01/98 **************************************
     C***********          Z-ADDEOCO#     KKCO#
     C***********          Z-ADDEOSUB#    KKSUB#
     C***********          Z-ADDEOFND     KKFND
     C***********          Z-ADDEOFYR     KKFYR
     C***********          Z-ADDEOCASE    KKCASE
     C***********          Z-ADD*ZEROS    KKEOB#
     C*** END   DEL *** 12/01/98 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   Z-ADD     *ZEROS        KKEOB#
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C     SSEOB#        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  '*ALL'        SCEOB#
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SSEOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL(P)  ##NUM         SCEOB#
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ZEROS        KKSVDT
     C                   MOVEL     *BLANKS       KKSTAF
     C                   MOVEL     *BLANKS       KKMSCD
     C                   MOVE      *ZEROS        KKMSEQ
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   Z-ADD     *ZEROS        KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        SRDTI
     C   21              CALL      'SRCVTDAT'    DTCYMD
     C   21              MOVEL     SRDTO         KKSVDT
     C*
     C   22              MOVEL     SCPST2        KKSTAF
     C   23              MOVEL     SCPST3        KKMSCD
     C   23              MOVE      SCPSTS        KKMSEQ
     C*** BEGIN ADD *** 08/04/98 **************************************
     C                   Z-ADD     *ZEROS        KKSEQ#
     C*** END   ADD *** 08/04/98 **************************************
     C*** BEGIN ADD *** 11/09/99 **************************************
     C                   Z-ADD     *ZEROS        KKEOB#
     C                   Z-ADD     *ZEROS        KKSEQ#
     C*** END   ADD *** 11/09/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVE      SCSVDT        KKSVDT
     C   22              MOVEL     SCSTAF        KKSTAF
     C   23              MOVEL     SCMSCD        KKMSCD
     C   23              MOVE      SCMSEQ        KKMSEQ
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   Z-ADD     SCEOMB        KKEOB#
     C                   Z-ADD     SCSEQ#        KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KHDO - F8= Claim Summary
     C*================================================================
     C     KHDO          BEGSR
     C*
     C                   CALL      'F3040R02'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C*** BEGIN ADD *** 10/20/99 **************************************
     C                   PARM      SSSYS         ##SYS             1
     C                   PARM      SSINVD        ##INVD            1
     C                   PARM      SSEOB#        ##EOB#            9 0
     C*** END   ADD *** 10/20/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      @@RCN         ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*** BEGIN ADD *** 08/06/06 **************************************
     C                   PARM      SSBFLG        ##BFLG            1
     C*** END   ADD *** 08/06/06 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - Searches for subset fields.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SSMSCD'
     C*
     C                   CALL      'F2425R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PMC01                              99
     C*
     C                   MOVEL     MCKEY         SSMSCD
     C                   MOVEL     MCDESC        SSDESC
     C*
     C                   SETON                                        32
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*================================================================
     C* KUDO - F20=RELEASE ALL
     C*================================================================
     C     KUDO          BEGSR
     C*
     C     TMKY1         CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      SUBHR
     C*
     C     MEETHR        IFEQ      *ON
     C*
     C                   MOVEL(P)  'R'           TMHOLD
     C*
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1PGM         TMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C                   MOVEL     DSTIME        TMTIME
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PTM01
     C*
     C     TMKY1         READE     PTM01                                  99
     C*
     C                   ENDDO
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=HOLD ALL
     C*================================================================
     C     KVDO          BEGSR
     C*
     C     TMKY1         CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      SUBHR
     C*
     C     MEETHR        IFEQ      *ON
     C*
     C                   MOVEL(P)  'H'           TMHOLD
     C*
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1PGM         TMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C                   MOVEL     DSTIME        TMTIME
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PTM01
     C*
     C     TMKY1         READE     PTM01                                  99
     C*
     C                   ENDDO
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDSR
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 01/21/00 **************************************
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C                   EXSR      ASSRRN
     C*
     C                   ENDIF                                                  VLDOPT<>Y
     C***  END   ADD  ***  01/21/00  *********************************
     C*
     C                   Z-ADD     TMSVDT        SCSVDT
     C                   MOVEL     TMSTAF        SCSTAF
     C                   MOVEL     TMMSCD        SCMSCD
     C                   Z-ADD     TMMSEQ        SCMSEQ
     C*** BEGIN ADD *** 08/04/98 **************************************
     C                   Z-ADD     TMSEQ#        SCSEQ#
     C                   Z-ADD     TMEOB#        SCEOMB
     C*** END   ADD *** 08/04/98 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   Z-ADD     TMRCN         SCRCN
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 08/06/06 **************************************
     C     *IN41         IFEQ      *ON
     C                   MOVEL     TMBFLG        SCBFLG
     C                   ENDIF
     C*** END   ADD *** 08/06/06 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL     TMHOLD        SCHOLD
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCHOLD
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C     EOKY2         CHAIN     PEO02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN5        SCRRN
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        SCRRN
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C                   Z-ADD     TMSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSVDT            8
     C*
     C                   MOVEL     XXSVDT        ##NUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C*
     C   21              MOVEL     ##NUM         X1SVDT
     C   22              MOVEL     ##NUM         X2SVDT
     C   23              MOVEL     ##NUM         X3SVDT
     C*
     C   21              MOVEL     TMSTAF        X1STAF
     C   22              MOVEL     TMSTAF        X2STAF
     C   23              MOVEL     TMSTAF        X3STAF
     C*
     C   21              MOVEL     TMMSCD        X1MSCD
     C   22              MOVEL     TMMSCD        X2MSCD
     C   23              MOVEL     TMMSCD        X3MSCD
     C*
     C                   MOVEL     TMMSEQ        ##NUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C   21              MOVEL(P)  ##NUM         X1MSEQ
     C   22              MOVEL(P)  ##NUM         X2MSEQ
     C   23              MOVEL(P)  ##NUM         X3MSEQ
     C*
     C   21              MOVE      X1DAT         SCDATA
     C   22              MOVE      X2DAT         SCDATA
     C   23              MOVE      X3DAT         SCDATA
     C*
     C*** BEGIN DEL *** 08/14/00 **************************************
     C**********           MOVELTMDESC    SCDESC
     C**********           MOVELTMACT     SCACT
     C*** END   DEL *** 08/14/00 **************************************
     C*** BEGIN ADD *** 08/14/00 **************************************
     C                   MOVEL     TMDESC        XXDESC           15
     C*
     C                   MOVEL     TMEOB#        ##NUM
     C*
     C                   MOVE      TMSYS         XXSYS             2
     C*
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C                   MOVEL     ##NUM         XXEOB#           14
     C*
     C                   SELECT
     C*
     C     INFO#         WHENEQ    1
     C                   MOVEL(P)  TMACT         SCDAT2
     C                   MOVE      XXDESC        SCDAT2
     C*
     C     INFO#         WHENEQ    2
     C                   MOVEL(P)  XXSYS         SCDAT2
     C                   MOVE      XXEOB#        SCDAT2
     C*
     C                   ENDSL
     C*** END   ADD *** 08/14/00 **************************************
     C                   MOVE      TMMILE        SCMILE
     C                   MOVE      TMHRS         SCHRS
     C                   MOVE      TMMSCG        SCMSCG
     C*** BEGIN ADD *** 09/27/99 **************************************
     C*
     C                   Z-ADD     TMEOB#        KKEOB#
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C*** BEGIN CHG *** 12/01/98 **************************************
     C********** EOBLFL    IFEQ 'P'
     C     EOINV#        IFNE      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*** END   CHG *** 12/01/98 **************************************
     C*
     C                   MOVEL     'Y'           SCINVD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'N'           SCINVD
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*
     C* RESET ACDPL'S PARM ##RRN2 TO THE VALUE PASSED INTO THIS PGM.
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C*
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*================================================================
     C* OPT21 - HOLD/RELEASE
     C*================================================================
     C     OPT21         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKH         CASEQ     *ON           KHDO
     C     *INKN         CASEQ     *ON           KNDO
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     *INKU         CASEQ     *ON           KUDO
     C     *INKV         CASEQ     *ON           KVDO
     C*** END   ADD *** 10/25/99 **************************************
     C                   CAS                     KEDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     SCSEL         CASEQ     '21'          OPT21
     C                   ENDCS
     C*** END   ADD *** 10/25/99 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     @@RRN2        CHAIN     PEO01                              99
     C*** BEGIN ADD *** 12/01/98 **************************************
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 12/01/98 **************************************
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*** BEGIN ADD *** 05/02/05 **************************************
     C                   Z-ADD     EOEOB#        K1EOB#
     C*** END   ADD *** 05/02/05 **************************************
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C*** BEGIN CHG *** 08/13/98 **************************************
     C********** KEY1      CHAINDBRCDF               99
     C*** BEGIN CHG *** 09/27/99 **************************************
     C********** TMKY      CHAINDBRCDF               99
     C*** BEGIN CHG *** 10/25/99 **************************************
     C********** TMKY      CHAINPTM01                99
     C     TMKY          CHAIN(N)  PTM01                              99
     C*** END   CHG *** 10/25/99 **************************************
     C*** END   CHG *** 09/27/99 **************************************
     C*** END   CHG *** 08/13/98 **************************************
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'F'           @@CALL
     C*** BEGIN ADD *** 12/01/98 **************************************
     C                   MOVEL(P)  ' '           EOFLG5
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'Y'           EOFLG5
     C*** END   ADD *** 12/01/98 **************************************
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 12/01/98 **************************************
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
     C*** END   ADD *** 12/01/98 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ELSE
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C  N99EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*** BEGIN ADD *** 05/02/05 **************************************
     C                   Z-ADD     EOEOB#        K1EOB#
     C*** END   ADD *** 05/02/05 **************************************
     C*
     C*** BEGIN CHG *** 10/25/99 **************************************
     C********** TMKY      CHAINPTM01                99
     C     TMKY          CHAIN(N)  PTM01                              99
     C*** END   CHG *** 10/25/99 **************************************
     C*
     C     *IN99         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 09/04/01 **************************************
     C**********           MOVEL'F'       @@CALL
     C**********           MOVEL' '       EATMFL    P
     C     EASYS         IFEQ      'M'
     C     EABINV        ANDEQ     *ZEROS
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EASYS         OREQ      'M'
     C     EABINV        ANDEQ     *ZEROS
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C                   DELETE    PEA01
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PEO01
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'F'           @@CALL
     C                   MOVEL(P)  ' '           EATMFL
     C                   UPDATE    PEA01
     C                   ENDIF
     C*** END   CHG *** 09/04/01 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'Y'           EATMFL
     C*** BEGIN ADD *** 01/06/99 **************************************
     C     EABINV        IFEQ      *ZEROS
     C     EABAMT        ANDNE     *ZEROS
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDNE     'P'
     C     EABAMT        ANDNE     *ZEROS
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   MOVEL(P)  *BLANKS       EABBLF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/06/99 **************************************
     C*** BEGIN ADD *** 09/04/01 **************************************
     C                   UPDATE    PEA01
     C*** END   ADD *** 09/04/01 **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 09/04/01 **************************************
     C**********           UPDATPEA01
     C*** END   DEL *** 09/04/01 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 03/13/00 **************************************
     C*** BEGIN ADD *** 01/31/03 **************************************
     C     HDSYS         IFEQ      'M'
     C     HDSYS         OREQ      'U'
     C*** END   ADD *** 01/31/03 **************************************
     C                   CALL      'FA724R'
     C                   PARM      @@RRN2        ##RRN             9 0           1
     C                   PARM      *BLANKS       ##RTCD            1             2
     C*** BEGIN ADD *** 01/31/03 **************************************
     C                   ENDIF
     C*** END   ADD *** 01/31/03 **************************************
     C*** END   ADD *** 03/13/00 **************************************
     C*
     C                   MOVEL     *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C*** END   ADD *** 12/14/01 **************************************
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C                   MOVEA     HDR           HD
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C                   MOVEL     CLRFKY        OFD(5)
     C*** BEGIN CHG *** 08/14/00 **************************************
     C**********           MOVELCLRFKY    OFD,14
     C**********           MOVELCLRFKY    OFD,15
     C                   MOVEL     CLRFKY        OFD(15)
     C                   MOVEL     CLRFKY        OFD(16)
     C*** END   CHG *** 08/14/00 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C* LOAD ADDITIONAL HEADER VALUES
     C*
     C                   MOVEL     EOSYS         SCSYS
     C                   MOVEL(P)  EOSYS         KKTKEY
     C                   MOVEL     'SYS'         KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTDESC        SCSYNM
     C*
     C                   MOVEL(P)  EOEOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL(P)  ##NUM         SCEOB#
     C*
     C     Q1PRMS        IFGT      5
     C*
     C                   Z-ADD     @@RRN3        ##RRN3
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ##RRN3
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   SELECT
     C*
     C     @@CERT        WHENNE    *BLANKS
     C*
     C                   MOVEL(P)  @@CERT        KKCERT
     C                   Z-ADD     @@EXT         KKEXT
     C*
     C     EOKY1         CHAIN     PEO03                              99
     C*
     C                   Z-ADD     DBRRN4        ##RRN2
     C*
     C     EOINV#        IFNE      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     Q1PRMS        IFGT      8
     C     @@EOB         IFNE      'C'
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C                   MOVEL     CLRFKY        OFD(2)
     C                   MOVEL     CLRFKY        OFD(3)
     C*** BEGIN ADD *** 01/11/00 **************************************
     C                   MOVEL     CLRFKY        OFD(8)
     C*** END   ADD *** 01/11/00 **************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           MOVELCLRFKY    OFD,7
     C*** BEGIN DEL *** 01/11/00 **************************************
     C**********           MOVELCLRFKY    OFD,8
     C*** END   DEL *** 01/11/00 **************************************
     C*** END   CHG *** 10/25/99 **************************************
     C*
     C                   ENDIF
     C*
     C     @@CERT        WHENEQ    *BLANKS
     C*
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*** BEGIN DEL *** 01/11/00 **************************************
     C********** Q1PRMS    IFGT 8
     C********** @@EOB     IFEQ 'C'
     C*
     C**********           MOVELCLRFKY    OFD,8
     C*
     C**********           ENDIF
     C*
     C**********           ENDIF
     C*** END   DEL *** 01/11/00 **************************************
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C*
     C                   ENDSL
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     Q1PRMS        IFGT      8
     C     @@EOB         IFEQ      'C'
     C*
     C                   MOVEL     CLRFKY        OFD(5)
     C*** BEGIN CHG *** 08/14/00 **************************************
     C**********           MOVELCLRFKY    OFD,14
     C**********           MOVELCLRFKY    OFD,15
     C                   MOVEL     CLRFKY        OFD(15)
     C                   MOVEL     CLRFKY        OFD(16)
     C*** END   CHG *** 08/14/00 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 08/04/06 **************************************
     C                   SETOFF                                       41
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C     XXCLT#        IFEQ      '139'
     C                   SETON                                        41
     C                   ENDIF
     C*** END   ADD *** 08/04/06 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C   21KEY5          READE     PTM02                                  95
     C   22KEY5          READE     PTM03                                  95
     C   23KEY5          READE     PTM04                                  95
      /Free
       dbRRN  = dbRRNa;
       dbRRN2 = dbRRNb;
       dbRRN3 = dbRRNc;
      /End-free
     C*
     C     SSINVD        WHENEQ    'N'
     C                   Z-ADD     *ZEROS        KKINV#
     C   21KEY9          READE     PTM05                                  95
     C   22KEY9          READE     PTM06                                  95
     C   23KEY9          READE     PTM07                                  95
      /Free
       dbRRN  = dbRRNd;
       dbRRN2 = dbRRNe;
       dbRRN3 = dbRRNf;
      /End-free
     C*
     C                   OTHER
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C   23KEY1          READE     DBRCDF3                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C   21KEY5          READPE    PTM02                                  95
     C   22KEY5          READPE    PTM03                                  95
     C   23KEY5          READPE    PTM04                                  95
      /Free
       dbRRN  = dbRRNa;
       dbRRN2 = dbRRNb;
       dbRRN3 = dbRRNc;
      /End-free
     C*
     C     SSINVD        WHENEQ    'N'
     C                   Z-ADD     *ZEROS        KKINV#
     C   21KEY9          READPE    PTM05                                  95
     C   22KEY9          READPE    PTM06                                  95
     C   23KEY9          READPE    PTM07                                  95
      /Free
       dbRRN  = dbRRNd;
       dbRRN2 = dbRRNe;
       dbRRN3 = dbRRNf;
      /End-free
     C*
     C                   OTHER
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C   23KEY1          READPE    DBRCDF3                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*** BEGIN CHG *** 01/21/00 **************************************
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C   21KEY6          SETGT     PTM02                              96
     C   22KEY7          SETGT     PTM03                              96
     C   23KEY8          SETGT     PTM04                              96
     C*
     C     SSINVD        WHENEQ    'N'
     C                   Z-ADD     *ZEROS        KKINV#
     C   21KEY10         SETGT     PTM05                              96
     C   22KEY11         SETGT     PTM06                              96
     C   23KEY12         SETGT     PTM07                              96
     C*
     C                   OTHER
     C*** END   ADD *** 01/21/00 **************************************
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C   23KEY4          SETGT     DBRCDF3                            96
     C*** BEGIN ADD *** 01/21/00 **************************************
     C                   ENDSL
     C*** END   ADD *** 01/21/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*** BEGIN ADD *** 08/06/06 **************************************
     C     *IN41         IFEQ      *OFF
     C                   MOVE      '   '         SCHDL1
     C                   ENDIF
     C*** END   ADD *** 08/06/06 **************************************
     C*** BEGIN ADD *** 01/21/00 **************************************
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C   21KEY6          SETLL     PTM02                              96
     C   22KEY7          SETLL     PTM03                              96
     C   23KEY8          SETLL     PTM04                              96
     C*
     C     SSINVD        WHENEQ    'N'
     C                   Z-ADD     *ZEROS        KKINV#
     C   21KEY10         SETLL     PTM05                              96
     C   22KEY11         SETLL     PTM06                              96
     C   23KEY12         SETLL     PTM07                              96
     C*
     C                   OTHER
     C*** END   ADD *** 01/21/00 **************************************
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C   23KEY4          SETLL     DBRCDF3                            96
     C*** BEGIN ADD *** 01/21/00 **************************************
     C                   ENDSL
     C*** END   ADD *** 01/21/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*** BEGIN CHG *** 08/14/00 **************************************
     C**********           MOVE DL        SCDESC    P
     C                   MOVE(P)   DL            SCDAT2
     C*** END   CHG *** 08/14/00 **************************************
     C                   MOVE(P)   DL            SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*================================================================
     C* SUBHR - Determine if record should be held or released
     C*================================================================
     C     SUBHR         BEGSR
     C*
     C                   MOVEL     *ON           MEETHR            1
     C*
     C     SSSVD1        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSSVD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVD1
     C*
     C     XXSVD1        IFGT      TMSVDT
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SSSVD2        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSSVD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVD2
     C*
     C     XXSVD2        IFLT      TMSVDT
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SSMSCD        IFNE      *BLANKS
     C     SSMSCD        ANDNE     TMMSCD
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C                   MOVE      TMEOB#        KKEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     ##PRVB        IFNE      'Y'
     C*
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSSYS         ANDNE     EOSYS
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     TMEOB#        KKEOB#
     C                   Z-ADD     TMRCN         KKRCN
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   ENDIF
     C*
     C     EOINV#        IFNE      *ZEROS
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   MOVEL     'Y'           XXINVD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'N'           XXINVD
     C*
     C                   ENDIF
     C*
     C     XXINVD        IFEQ      'Y'
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSEOB#        IFNE      *ZEROS
     C     SSEOB#        ANDNE     TMEOB#
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSCERT        IFNE      *BLANKS
     C     SSCERT        ANDNE     EOCERT
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSEXT         IFNE      *ZEROS
     C     SSEXT         ANDNE     EOEXT
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSSEQ#        IFNE      *ZEROS
     C     SSSEQ#        ANDNE     TMMSEQ
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSRCN         IFNE      *BLANKS
     C*
     C                   MOVE      SSRCN         XXRCN
     C*
     C     XXRCN         IFNE      TMRCN
     C*
     C                   MOVE      *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSVD1        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSSVD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVD1
     C*
     C     XXSVD1        IFGT      TMSVDT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SSSVD2        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSSVD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVD2
     C*
     C     XXSVD2        IFLT      TMSVDT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SSMSCD        IFNE      *BLANKS
     C     SSMSCD        ANDNE     TMMSCD
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   MOVE      TMEOB#        KKEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*** BEGIN ADD *** 09/27/99 **************************************
     C*
     C     ##PRVB        IFNE      'Y'
     C*
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSSYS         ANDNE     EOSYS
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C*
     C                   Z-ADD     TMEOB#        KKEOB#
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     EAKY          CHAIN(N)  PEA01                              99
     C*
     C                   ELSE
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     SSINVD        IFNE      *BLANKS
     C*
     C*** BEGIN CHG *** 12/01/98 **************************************
     C********** EOBLFL    IFEQ 'P'
     C     EOINV#        IFNE      *ZEROS
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        ANDNE     'Y'
     C     EABINV        ORNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*** END   ADD *** 09/27/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     EABINV        OREQ      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##PRVB        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*** END   CHG *** 12/01/98 **************************************
     C*
     C                   MOVEL     'Y'           XXINVD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'N'           XXINVD
     C*
     C                   ENDIF
     C*
     C     SSINVD        IFNE      XXINVD
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SSEOB#        IFNE      *ZEROS
     C     SSEOB#        ANDNE     TMEOB#
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/19/98 **************************************
     C*
     C     SSCERT        IFNE      *BLANKS
     C     SSCERT        ANDNE     EOCERT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSEXT         IFNE      *ZEROS
     C     SSEXT         ANDNE     EOEXT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSSEQ#        IFNE      *ZEROS
     C     SSSEQ#        ANDNE     TMMSEQ
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C*
     C     SSRCN         IFNE      *BLANKS
     C*
     C                   MOVE      SSRCN         XXRCN
     C*
     C     XXRCN         IFNE      TMRCN
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/17/99 **************************************
     C*** BEGIN ADD *** 08/06/06 **************************************
     C     SSBFLG        IFNE      *BLANKS
     C     SSBFLG        ANDNE     TMBFLG
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 08/06/06 **************************************
     C*
     C                   ENDSR
** Column headings for Rehab
Opt    Date    Svc   Seq# Stf Act Description     Mile Hours   Charge Inv Hld
Opt Stf    Date    Svc   Seq# Act Description     Mile Hours   Charge Inv Hld
Opt Svc   Seq#    Date    Stf Act Description     Mile Hours   Charge Inv Hld
Opt    Date    Svc   Seq# Stf Sys   EOB#/Svc#     Mile Hours   Charge Inv Hld
Opt Stf    Date    Svc   Seq# Sys   EOB#/Svc#     Mile Hours   Charge Inv Hld
Opt Svc   Seq#    Date    Stf Sys   EOB#/Svc#     Mile Hours   Charge Inv Hld
** Column headings
Opt    Date    Svc   Seq# Stf Act Description     Mile Hours   Charge Inv   BF
Opt Stf    Date    Svc   Seq# Act Description     Mile Hours   Charge Inv   BF
Opt Svc   Seq#    Date    Stf Act Description     Mile Hours   Charge Inv   BF
Opt    Date    Svc   Seq# Stf Sys   EOB#/Svc#     Mile Hours   Charge Inv   BF
Opt Stf    Date    Svc   Seq# Sys   EOB#/Svc#     Mile Hours   Charge Inv   BF
Opt Svc   Seq#    Date    Stf Sys   EOB#/Svc#     Mile Hours   Charge Inv   BF
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12121=Hold/Release          YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
208F8=Claim Summary         YYY
209F9=Change Info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
214F14=DARTS                YYY
217F17=Subset               YYY
220F20=Release All          YNN
221F21=Hold All             YNN
3
