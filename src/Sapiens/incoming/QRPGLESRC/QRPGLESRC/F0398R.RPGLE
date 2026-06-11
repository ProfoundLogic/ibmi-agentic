     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* CREATED: 05/16/03  C:079
     H* PROJ: FS249   WORK WITH PENDED ITEMS BASED ON F0391R
     H*****************************************************************
     H* REVISIONS:
     H*
     H*  06/14/13  C:079  REDMINE #3140
     H*                   Trap error on delete override
     H*
     H*  05/01/03  C:079  PROJECT FS259
     H*                   ADD OPTION 12=DISPLAY STATE RULE AND 13=
     H*                   CORRECT CODING RULE
     H*
     H*  01/07/03  C:079  PROJECT 114MDC
     H*                   SELECT LINES ON BILLS PENDED BY CODEREVIEW
     H*
     H*  08/19/02  C:062  PROJECT MCMNT 33877
     H*                   INITIALIZE XXPMOD.
     H*
     H*  03/28/02  C:079  PROJECT:MCMNT 31837
     H*                   USE DHMODx to pop proc code mod instead of
     H*                   CAPMOD
     H*
     H*  02/26/02  C:062  PROJECT:MC025 31413
     H*                   CALL SYSTEM API TO GET VALUES FOR ##WCIN AND
     H*                   ##WCDK
     H*
     H*  02/12/02  C:079  PROJECT:MC025 31229
     H*                   Initialize RRN for FAEOP/FDD*P on ACDPL
     H*
     H*  02/08/02  C:062  PROJECT:MC044 31041
     H*                   COMPILE ONLY.
     H*
     H*  02/03/02  C:079  PROJECT:MC044 31045
     H*                   1. DO NOT DISPLAY ADJUDICATION ERROR MESSAGE
     H*
     H*  01/16/02 C:079 PROJECT:114MAO
     H*                 IF CALLED FROM CORRECT CODING PASS SERVICE
     H*                 DATE.
     H*
     H*  11/19/01 C:062 PROJECT:MC010 29804
     H*                 ADD FILE OVERRIDES TO FMPRP/FMSVP/FTABL
     H*
     H*  05/24/01 C:062 PROJECT:FSMNT #192  BASE ONLY
     H*                 ADD PARM TO CALL OF F9951R, TO LET IT KNOW
     H*                 THAT IT IS NOT BEING CALLED FROM THE MENU.
     H*
     H*****************************************************************
     FF0391F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDCAP     IF   E             DISK
     F                                     RENAME(PCA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFDCAL23   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCA01:DBRCDF)
     F*
     F*  Second logical file definition (for displaying items)
     FFDCALV    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCA01:DBRCDF2)
     F*
     F*  Third logical file definition (for displaying items)
     FFDCAL24   IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCA01:DBRCDF3)
     F*
     F*  Fourth logical file definition (for displaying items)
     FFDCAL25   IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PCA01:DBRCDF4)
     F*
     F*  Subset logical file (EOB#/FDAT)
     FFDCALH    IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F                                     RENAME(PCA01:DBRCDFA)
     F*
     F*  Subset logical file (EOB#/CODE)
     FFDCALW    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F                                     RENAME(PCA01:DBRCDFB)
     F*
     F*  Subset logical file (FEIN/MOD/FDAT)
     FFDCALU    IF   E           K DISK
     F                                     INFDS(DBFDSC)
     F                                     RENAME(PCA01:DBRCDFC)
     F*
     F*  Subset logical file (FEIN/MOD/CODE)
     FFDCALX    IF   E           K DISK
     F                                     INFDS(DBFDSD)
     F                                     RENAME(PCA01:DBRCDFD)
     F*
     F*  Subset logical file (FEIN/MOD/EOB#)
     FFDCALL    IF   E           K DISK
     F                                     INFDS(DBFDSE)
     F                                     RENAME(PCA01:DBRCDFE)
     F*
     F*  EOMB Phys/HCFA Detail Transaction File
     FFDDHL     IF   E           K DISK
     F                                     INFDS(DBFD1)
     F*  EOMB Hosp/UB Bill Detail Trans File
     FFDDUL     IF   E           K DISK
     F                                     INFDS(DBFD2)
     F*  EOMB Pharmacy Bill Detail Trans File
     FFDDPL     IF   E           K DISK
     F                                     INFDS(DBFD3)
     F*  EOMB Dental Bill Detail Trans File
     FFDDDL     IF   E           K DISK
     F                                     INFDS(DBFD4)
     F*  EOMB Transaction File
     FFAEOL31   IF   E           K DISK
     F                                     INFDS(DBFD5)
     F*  Provider Bills Control File
     FFMBEL     IF   E           K DISK
     F*  Provider Bills msg database
     FFDMGL1    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCXL     IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     F*** BEG ADD *** 05/01/03 ****************************************
     FFDFRL0    IF   E           K DISK
     F                                     INFDS(DBFD6)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*** END ADD *** 05/01/03 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(8) CTDATA PERRCD(1)              Column headings
     D*** BEG CHG *** 05/01/03 ****************************************
     D******************* OF      1  21  1   OFD    35   Options, Function
     D OF              S              1    DIM(23) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(23) ALT(OF)
     D*** END CHG *** 05/01/03 ****************************************
     D MSG             S             65    DIM(1) CTDATA PERRCD(1)
     D*** BEGIN ADD *** 11/19/01 **************************************
     D QCA             S             56    DIM(14) CTDATA PERRCD(1)             CMD EXEC/QCMDEXC
     D*** END   ADD *** 11/19/01 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D* RRN# Assignment
     D*================================================================
     D* FDDHP - Physician
     D DBFD1           DS
     D  DFRRN1               397    400B 0
     D* FDDUP - Hospital
     D DBFD2           DS
     D  DFRRN2               397    400B 0
     D* FDDPP - Pharmacy
     D DBFD3           DS
     D  DFRRN3               397    400B 0
     D* FDDDP - Dental
     D DBFD4           DS
     D  DFRRN4               397    400B 0
     D* FAEOP - EOMB Transaction File
     D DBFD5           DS
     D  DFRRN5               397    400B 0
     D*** BEG ADD *** 05/01/03 ****************************************
     D DBFD6           DS
     D  DFRRN6               397    400B 0
     D*** END ADD *** 05/01/03 ****************************************
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
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pended Items')
     D OPTNM2          C                   CONST('     Provider Discou-
     D                                     nt Information      ')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(4)
     D SF1PAG          C                   CONST(6)
     D*================================================================
     D* DATA STRUCTURES FOR SCREEN FIELDS
     D*================================================================
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11
     D* View #1
     D                 DS
     D  VIEW1                  1     53
     D  V1SLI                  1      1
     D  V1DOS                  3     12
     D  V1CODE                14     25
     D  V1MD                  27     29
     D  V1EOB#                31     39
     D  V1FEIN                41     49
     D  V1HYPH                50     50
     D  V1MOD                 51     53
     D*
     D* View #2
     D                 DS
     D  VIEW2                  1     53
     D  V2SLI                  1      1
     D  V2CODE                 3     14
     D  V2MD                  16     18
     D  V2EOB#                20     28
     D  V2FEIN                30     38
     D  V2HYPH                39     39
     D  V2MOD                 40     42
     D  V2DOS                 44     53
     D*
     D* View #3
     D                 DS
     D  VIEW3                  1     53
     D  V3SLI                  1      1
     D  V3EOB#                 3     11
     D  V3FEIN                13     21
     D  V3HYPH                22     22
     D  V3MOD                 23     25
     D  V3DOS                 27     36
     D  V3CODE                38     49
     D  V3MD                  51     53
     D*
     D* View #4
     D                 DS
     D  VIEW4                  1     53
     D  V4SLI                  1      1
     D  V4FEIN                 3     11
     D  V4HYPH                12     12
     D  V4MOD                 13     15
     D  V4DOS                 17     26
     D  V4CODE                28     39
     D  V4MD                  41     43
     D  V4EOB#                45     53
     D*
     D                 DS
     D  INFO1                  1     21
     D  I1CHRG                 1     10
     D  I1PAID                12     21
     D                 DS
     D  INFO2                  1     21
     D  I2DTEN                 3     14
     D                 DS
     D  ##NUM                  1     80
     D  XXNUM                  6      9
     D*
     D*** BEGIN ADD *** 11/19/01 **************************************
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D                 DS
     D  QQCMD2                 1     56
     D  QQST2                 40     41

     I*** END   ADD *** 11/19/01 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C*** BEG ADD *** 01/16/02 ****************************************
     C                   PARM                    @@FDAT            7 0
     C*** END ADD *** 01/16/02 ****************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C*** BEG DEL *** 02/12/02 ****************************************
     C********** ACDPL     PLIST
     C********** *INKC     PARM           ##KC    1
     C********** *INKL     PARM           ##KL    1
     C**********           PARM           ##MODE  1
     C**********           PARM           ##RRN   90       FDDxP
     C**********           PARM           ##RRN2  90       FAEOP
     C**********           PARM SCDBRR    ##RRN3  90       FDCAP
     C**********           PARM           ##NWCD  5
     C*** END DEL *** 02/12/02 ****************************************
     C*
     C*** BEG ADD *** 02/12/02 ****************************************
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBR1        ##RRN             9 0          FDDxP
     C                   PARM      SCDBR2        ##RRN2            9 0          FAEOP
     C                   PARM      SCDBRR        ##RRN3            9 0          FDCAP
     C                   PARM                    ##NWCD            5
     C*** END ADD *** 02/12/02 ****************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
     C     C2UMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM                    ##DEC             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFDAT
     C                   KFLD                    KKDSEQ
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCODE
     C                   KFLD                    KKDSEQ
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKDSEQ
     C*
     C     KEY3A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C*
     C     KEY4A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSFEIN
     C                   KFLD                    SSMOD
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C                   KFLD                    KKFDAT
     C                   KFLD                    KKDSEQ
     C*
     C     KEY2B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSEOB#
     C                   KFLD                    KKCODE
     C                   KFLD                    KKDSEQ
     C*
     C     KEY3B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSFEIN
     C                   KFLD                    SSMOD
     C                   KFLD                    KKFDAT
     C                   KFLD                    KKDSEQ
     C*
     C     KEY4B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSFEIN
     C                   KFLD                    SSMOD
     C                   KFLD                    KKCODE
     C                   KFLD                    KKDSEQ
     C*
     C     KEY5B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SSFEIN
     C                   KFLD                    SSMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C* FDDHP - Physician
     C     DHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    SCDSEQ
     C*
     C* FDDUP - Hospital
     C     DUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    SCDSEQ
     C*
     C* FDDPP - Pharmacy
     C     DPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    SCDSEQ
     C*
     C* FDDDP - Dental
     C     DDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    SCDSEQ
     C*
     C* FAEOP - EOB Transaction File
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKEOB#
     C* FAEAP - EOB Header Amount Transaction File
     C     EAKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CARCN
     C*
     C* FMBEP - Provider bills control file
     C     BEKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C* FDMGL1 - Provider Bills error msg database
     C     MGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*** BEG ADD *** 05/01/03 ****************************************
     C*
     C* STATE RULES
     C*
     C     FRKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKRULE
     C                   KFLD                    KKEFDT
     C*
     C     FRKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKRULE
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     DHKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    HLDSEQ
     C*** END ADD *** 05/01/03 ****************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACODE        KKCODE
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    EOAST         KKAST
     C     *LIKE         DEFINE    EOSYS         KKSYS
     C     *LIKE         DEFINE    EOBTYP        KKBTYP
     C     *LIKE         DEFINE    EOSTYP        KKSTYP
     C*** BEG ADD *** 05/01/03 ****************************************
     C     *LIKE         DEFINE    CAAST         KKST
     C     *LIKE         DEFINE    CAFDAT        KKEFDT
     C     *LIKE         DEFINE    CARULE        KKRULE
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C*** END ADD *** 05/01/03 ****************************************
     C     *LIKE         DEFINE    EOFEIN        HLFEIN
     C     *LIKE         DEFINE    EOMOD         HLMOD
     C     *LIKE         DEFINE    EOSUB#        #2SUB#
     C     *LIKE         DEFINE    EOFND         #2FND
     C     *LIKE         DEFINE    EOFEIN        #1FEIN
     C     *LIKE         DEFINE    EOMOD         #1MOD
     C     *LIKE         DEFINE    EOACDT        ##ACDT
     C     *LIKE         DEFINE    EOSS#         ##SS#
     C*** BEG ADD *** 03/28/02 ****************************************
     C     *LIKE         DEFINE    DHMOD1        XXPMOD
     C*** END ADD *** 03/28/02 ****************************************
     C*
     C/COPY SCOPYWW01C
     C*
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
     C   23              Z-ADD     DBRRN3        SCDBRR
     C   24              Z-ADD     DBRRN3        SCDBRR
     C*
     C     SSFEIN        WHENNE    *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C   21              Z-ADD     DBRRNC        SCDBRR
     C   22              Z-ADD     DBRRND        SCDBRR
     C   23              Z-ADD     DBRRNE        SCDBRR
     C   24              Z-ADD     DBRRN4        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSEOB#        IFNE      *ZEROS
     C     SSFEIN        ORNE      *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 11/19/01 **************************************
     C*================================================================
     C* DELOVR - Deletes the overrides to various state specific files
     C*================================================================
     C     DELOVR        BEGSR
     C*
    C*** Beg Chg *** 06/14/13 ****************************************
     C************       Z-ADD     56            LENGTH
     C************       CALL      'QCMDEXC'                              60
     C                   Z-ADD     60            LENGTH
     C                   CALL(e)   'QCMDEXC'                              60
    C*** End Chg *** 06/14/13 ****************************************
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH
    C*** Beg Chg *** 06/14/13 ****************************************
     C**************     CALL      'QCMDEXC'                              60
     C                   CALL(e)   'QCMDEXC'                              60
    C*** End Chg *** 06/14/13 ****************************************
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH
    C*** Beg Add *** 06/14/13 ****************************************
     C                   if        %error
     C                   endif
    C*** End Add *** 06/14/13 ****************************************
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(11)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(12)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(13)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(14)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*** END   ADD *** 11/19/01 **************************************
     C*================================================================
     C* SSCLER - Clear subset description fields
     C*================================================================
     C     SSCLER        BEGSR
     C*
     C* CLEAR ALL SUBSET DESCRIPTIONS IF CORRESPONDING SUBSET FIELD
     C* IS EMPTY
     C*
     C     SSFEIN        IFEQ      *ZEROS
     C     SSMOD         ANDEQ     *ZEROS
     C                   CLEAR                   SSPDNM
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVE      'Y'           VLDOPT            1
     C*
     C                   MOVE      SCEOB#        KKEOB#
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         ##CO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   MOVE      EOAST         KKAST
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVE      EOSTYP        KKSTYP
     C                   Z-ADD     DFRRN5        ##RRN2
     C*** BEG DEL *** 02/12/02 ****************************************
     C**********
     C**********
     C***Check*to see which detail program to call
     C**********
     C****PHYSICIAN *** (VENDOR)
     C********** EOSYS     IFEQ 'P'
     C********** EOBTYP    ANDEQ'N'
     C********** EOSYS     OREQ 'V'
     C********** EOSYS     ANDEQ'V'
     C********** DHKY      CHAINPDH01                99    Detail
     C****BEG*CHG *** 02/12/02 ****************************************
     C**N99*************** Z-ADDDFRRN1    ##RRN   90
     C**N99*****           Z-ADDDFRRN1    SCDBR1  90
     C****END*CHG *** 02/12/02 ****************************************
     C********** BEKY      CHAINPBE01                98
     C**N98*****           MOVELBEDEPG    ACDPGM 10
     C**********           ENDIF                           EOSYS = 'P'
     C****HOSPITAL ***
     C********** EOSYS     IFEQ 'H'
     C********** EOBTYP    ANDEQ'H'                        Hospital
     C********** DUKY      CHAINPDU01                99    Detail
     C****BEG*CHG *** 02/12/02 ****************************************
     C**N99*************** Z-ADDDFRRN2    ##RRN
     C**N99*****           Z-ADDDFRRN2    SCDBR1
     C****END*CHG *** 02/12/02 ****************************************
     C********** BEKY      CHAINPBE01                98
     C**N98*****           MOVELBEDEPG    ACDPGM
     C**********           ENDIF                           EOSYS = 'H'
     C****PHARMACY ***
     C********** EOSYS     IFEQ 'P'
     C********** EOBTYP    ANDEQ'X'                        pharmacy
     C********** DPKY      CHAINPDP01                99    detail
     C****BEG*CHG *** 02/12/02 ****************************************
     C**N99*************** Z-ADDDFRRN3    ##RRN
     C**N99*****           Z-ADDDFRRN3    SCDBR1
     C****END*CHG *** 02/12/02 ****************************************
     C********** BEKY      CHAINPBE01                98
     C**N98*****           MOVELBEDEPG    ACDPGM
     C**********           ENDIF                           EOSYS = 'P'
     C****DENTAL ***
     C********** EOSYS     IFEQ 'P'
     C********** EOBTYP    ANDEQ'D'                        dental
     C********** DDKY      CHAINPDD01                99    detail
     C****BEG*CHG *** 02/12/02 ****************************************
     C**N99*************** Z-ADDDFRRN4    ##RRN
     C**N99*****           Z-ADDDFRRN4    SCDBR1
     C****END*CHG *** 02/12/02 ****************************************
     C********** BEKY      CHAINPBE01                98
     C**N98*****           MOVELBEDEPG    ACDPGM
     C**********           ENDIF                           EOSYS = 'P'
     C*** END DEL *** 02/12/02 ****************************************
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVEL     *OFF          STOP              1
     C     SCSEL         IFEQ      '7'
     C     EONTWK        ANDEQ     *BLANKS
     C                   EXFMT     WDW01
     C                   MOVEL     *ON           STOP
     C*                    SETON                     89
     C                   ENDIF
     C*** BEGIN ADD *** 11/19/01 **************************************
     C*
     C* OVERRIDE VARIOUS STATE SPECIC FILES.
     C*
     C     *IN16         IFEQ      *OFF
     C                   EXSR      OVRFIL
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 11/19/01 **************************************
     C*** BEG ADD *** 05/01/03 ****************************************
     C*
     C* Check to see if there is a rule, if not send message
     C*
     C     SCSEL         IFEQ      '12'
     C*
     C                   MOVEL(P)  SCAST         KKST
     C                   Z-ADD     CARULE        KKRULE
     C                   Z-ADD     CAFDAT        KKEFDT
     C*
     C     FRKY1         SETGT     PFR01
     C     FRKY2         READPE    PFR01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Check to see if there is a correct coding rule, if not send msg
     C*
     C     SCSEL         IFEQ      '13'
     C*
     C                   Z-ADD     SCDSEQ        HLDSEQ
     C*
     C     DHKY1         CHAIN     PDH01                              99
     C*
     C                   CALL      'FA714R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      DFRRN1        ##RRN                          FDDHP
     C                   PARM      *ZEROS        ##RRNC            9 0
     C*
     C     ##RRNC        IFEQ      0
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 05/01/03 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* PROCESS SUBSET SEARCHES
     C*
     C                   EXSR      OTHFKS
     C     *IN90         CABEQ     *ON           CK2END
     C*
     C* CLEAR SUBSET DESCRIPTION FIELDS
     C*
     C                   EXSR      SSCLER
     C*
     C* validate provider
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSMOD         ANDNE     *ZEROS
     C                   Z-ADD     SSFEIN        KKFEIN
     C                   MOVEL     SSMOD         KKMOD
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN
     C                   PARM      KKMOD         #1MOD
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFEQ      '0'
     C                   SETON                                        9078
     C                   GOTO      CK2END
     C                   ELSE
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSFEIN NE *ZEROS
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSMOD         ANDEQ     *ZEROS
     C                   Z-ADD     SSFEIN        KKFEIN
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN
     C                   PARM                    #1MOD
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFEQ      '0'
     C                   SETON                                        9078
     C                   GOTO      CK2END
     C                   ELSE
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSFEIN NE *ZEROS
     C*
     C     CK2END        ENDSR
     C*================================================================
     C* ERRCKM - BILL ERRORS.
     C*================================================================
     C     ERRCKM        BEGSR
     C*
     C                   SETOFF                                       20
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   Z-ADD     CAEOB#        KKEOB#
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEA     MSG           SCMSG
     C                   SETON                                        20
     C                   ELSE
     C                   SETOFF                                       20
     C                   ENDIF
     C*
     C                   MOVEL     *IN20         IN20
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKFDAT
     C                   MOVE      *LOVAL        KKCODE
     C                   MOVE      *LOVAL        KKEOB#
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C                   MOVE      *LOVAL        KKDSEQ
     C*
     C*** BEG ADD *** 01/16/02 ****************************************
     C*
     C* IF CALLED FROM CORRECT CODING THEN SET UP TO POSITION TO THE
     C* SERVICE DATE OF THE CURRENT LINE ITEM
     C*
     C     Q1PRMS        IFGT      9
     C                   Z-ADD     @@FDAT        KKFDAT
     C                   ENDIF
     C*** END ADD *** 01/16/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        SRDTI
     C   21              CALL      'SRCVTDAT'    DTCYMD
     C   21              MOVEL     SRDTO         KKFDAT
     C*
     C   22              MOVEL     SCPST2        KKCODE
     C   23              MOVE      SCPST3        KKEOB#
     C   24              MOVE      SCPST4        KKFEIN
     C   24              MOVE      SCPSTA        KKMOD
     C*
     C                   Z-ADD     0             KKDSEQ
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCDSEQ        KKDSEQ
     C                   MOVEL     SCDOS         KKFDAT
     C                   MOVEL     SCCODE        KKCODE
     C                   MOVEL     SCEOB#        KKEOB#
     C                   MOVEL     SCFEIN        KKFEIN
     C                   MOVEL     SCMOD         KKMOD
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* Indicators for cursor positioning
     C*
     C                   SETOFF                                       3031
     C*
     C* Provider Search
     C*
     C     HFLD          IFEQ      'SSFEIN'
     C     HFLD          OREQ      'SSMOD'
     C                   MOVE      SSFEIN        HLFEIN
     C                   MOVE      SSMOD         HLMOD
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   MOVE      HLFEIN        SSFEIN
     C                   MOVE      HLMOD         SSMOD
     C                   SETON                                        9030
     C                   ELSE
     C                   MOVE      ##FEIN        SSFEIN
     C                   MOVE      ##MOD         SSMOD
     C                   SETON                                        9031
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   Z-ADD     SSFEIN        KKFEIN
     C                   MOVEL     SSMOD         KKMOD
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN
     C                   PARM      KKMOD         #1MOD
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SSPDNM
     C                   ENDIF
     C*
     C                   ENDIF                                                  HGLD = SSFEIN
     C*
     C     HFLD          IFEQ      'SSEOB#'
     C                   SETON                                        3290
     C                   ENDIF                                                  HGLD = SSFEIN
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=Detail Charge List
     C*================================================================
     C     KVDO          BEGSR
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   CALL      'F9951R'
     C                   PARM      OPTNM         ##PRDS
     C                   PARM      '1'           ##CALL
     C                   PARM      KKSUB#        #2SUB#
     C                   PARM      KKFND         #2FND
     C                   PARM      CMSS#         ##SS#
     C                   PARM      CMACDT        ##ACDT
     C                   PARM      *ZEROS        #1FEIN
     C                   PARM      *BLANKS       ##MOD
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   PARM      'Y'           ##MODE
     C*** END   ADD *** 05/24/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   VIEW1
     C                   CLEAR                   VIEW2
     C                   CLEAR                   VIEW3
     C                   CLEAR                   VIEW4
     C                   CLEAR                   SCSLI
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C                   EXSR      ASSRRN
     C*
     C                   ENDIF                                                  VLDOPT<>Y
     C*
     C                   Z-ADD     CAFDAT        SCDOS
     C                   MOVEL     CACODE        SCCODE
     C                   MOVEL     CAEOB#        SCEOB#
     C                   MOVEL     CAFEIN        SCFEIN
     C                   MOVEL     CAMOD         SCMOD
     C                   MOVEL     CADSEQ        SCDSEQ
     C*** BEGIN ADD *** 11/19/01 **************************************
     C                   MOVEL(P)  CAAST         SCAST
     C*** END   ADD *** 11/19/01 **************************************
     C     CAEOB#        IFEQ      @@EOB#
     C                   MOVEL     '*'           SCSLI             1
     C                   ENDIF
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DFRRN5        SCDBR2
     C*
     C*** BEG ADD *** 02/12/02 ****************************************
     C*
     C                   Z-ADD     SCDSEQ        KKDSEQ
     C*
     C                   MOVE      EOAST         KKAST
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVE      EOSTYP        KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C  N98              MOVEL     BEDEPG        ACDPGM
     C*** BEGIN ADD *** 08/19/02 **************************************
     C                   MOVE      *BLANKS       XXPMOD
     C*** END   ADD *** 08/19/02 **************************************
     C*
     C* Physician
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'N'
     C     EOSYS         OREQ      'V'
     C     EOBTYP        ANDEQ     'V'
     C*
     C     DHKY          CHAIN     PDH01                              99
     C                   Z-ADD     DFRRN1        SCDBR1
     C*
     C*** BEG ADD *** 03/28/02 ****************************************
     C*** BEGIN DEL *** 08/19/02 **************************************
     C**********           MOVE *BLANKS   XXPMOD
     C*** END   DEL *** 08/19/02 **************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     DHMOD1        WHENNE    *BLANKS
     C                   MOVE      DHMOD1        XXPMOD
     C*
     C     DHMOD2        WHENNE    *BLANKS
     C                   MOVE      DHMOD2        XXPMOD
     C*
     C     DHMOD3        WHENNE    *BLANKS
     C                   MOVE      DHMOD3        XXPMOD
     C*
     C     DHMOD4        WHENNE    *BLANKS
     C                   MOVE      DHMOD4        XXPMOD
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD *** 03/28/02 ****************************************
     C*
     C                   ENDIF                                                  EOSYS = P
     C*
     C* Hospital
     C*
     C     EOSYS         IFEQ      'H'
     C     EOBTYP        ANDEQ     'H'
     C     DUKY          CHAIN     PDU01                              99
     C                   Z-ADD     DFRRN2        SCDBR1
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C* Pharmacy
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'X'
     C     DPKY          CHAIN     PDP01                              99
     C                   Z-ADD     DFRRN3        SCDBR1
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C* Dental
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'D'
     C     DDKY          CHAIN     PDD01                              99
     C                   Z-ADD     DFRRN4        SCDBR1
     C                   ENDIF                                                  EOSYS = 'P'
     C*** END ADD *** 02/12/02 ****************************************
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCDBR2
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* INFO #1 - Charged and Paid amounts
     C*
     C     INFO#         WHENEQ    1
     C     CACHRG        IFNE      *ZEROS
     C                   MOVEL(P)  CACHRG        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   Z-ADD     2             ##DEC
     C                   CALL      CHGNUM        C2UMPL
     C                   MOVEL     ##NUM         I1CHRG
     C                   ELSE
     C                   MOVE(P)   '0.00'        I1CHRG
     C                   ENDIF
     C*
     C     CAPAID        IFNE      *ZEROS
     C                   MOVEL(P)  CAPAID        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   Z-ADD     2             ##DEC
     C                   CALL      CHGNUM        C2UMPL
     C                   MOVEL     ##NUM         I1PAID
     C                   ELSE
     C                   MOVE(P)   '0.00'        I1PAID
     C                   ENDIF
     C*
     C                   MOVE(P)   INFO1         SCDATA
     C*
     C* INFO #2 - Date Entered
     C*
     C     INFO#         WHENEQ    2
     C                   Z-ADD     CARDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         I2DTEN
     C*
     C                   MOVEL(P)  I2DTEN        ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         I2DTEN
     C*
     C                   MOVE(P)   INFO2         SCDATA
     C*
     C                   ENDSL
     C*
     C* VIEW #1 - Date of Service
     C*
     C     *IN21         IFEQ      *ON
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         V1DOS
     C*
     C                   MOVEL(P)  V1DOS         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V1DOS
     C*
     C                   MOVEL(P)  CACODE        V1CODE
     C*** BEG CHG *** 03/28/02 ****************************************
     C******************** MOVELCAPMOD    V1MD
     C                   MOVEL     XXPMOD        V1MD
     C*** END CHG *** 03/28/02 ****************************************
     C*
     C                   MOVEL     CAEOB#        V1EOB#
     C                   MOVEL(P)  V1EOB#        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V1EOB#
     C*
     C                   MOVEL     CAFEIN        V1FEIN
     C                   MOVEL     CAMOD         V1MOD
     C                   MOVEL     SCSLI         V1SLI
     C*
     C                   MOVEL     VIEW1         SCDATA
     C                   ENDIF
     C*
     C* VIEW #2 - Code
     C*
     C     *IN22         IFEQ      *ON
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         V2DOS
     C*
     C                   MOVEL(P)  V2DOS         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V2DOS
     C*
     C                   MOVEL(P)  CACODE        V2CODE
     C*** BEG CHG *** 03/28/02 ****************************************
     C******************** MOVELCAPMOD    V2MD
     C                   MOVEL     XXPMOD        V2MD
     C*** END CHG *** 03/28/02 ****************************************
     C*
     C                   MOVEL     CAEOB#        V2EOB#
     C                   MOVEL(P)  V2EOB#        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V2EOB#
     C*
     C                   MOVEL     CAFEIN        V2FEIN
     C                   MOVEL     CAMOD         V2MOD
     C                   MOVEL     SCSLI         V2SLI
     C*
     C                   MOVEL     VIEW2         SCDATA
     C                   ENDIF
     C*
     C* VIEW #3 - EOMB#
     C*
     C     *IN23         IFEQ      *ON
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         V3DOS
     C*
     C                   MOVEL(P)  V3DOS         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V3DOS
     C*
     C                   MOVEL(P)  CACODE        V3CODE
     C*** BEG CHG *** 03/28/02 ****************************************
     C******************** MOVELCAPMOD    V3MD
     C                   MOVEL     XXPMOD        V3MD
     C*** END CHG *** 03/28/02 ****************************************
     C*
     C                   MOVEL     CAEOB#        V3EOB#
     C                   MOVEL(P)  V3EOB#        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V3EOB#
     C*
     C                   MOVEL     CAFEIN        V3FEIN
     C                   MOVEL     CAMOD         V3MOD
     C                   MOVEL     SCSLI         V3SLI
     C*
     C                   MOVEL     VIEW3         SCDATA
     C                   ENDIF
     C*
     C* VIEW #4 - FEIN/Mod
     C*
     C     *IN24         IFEQ      *ON
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         V4DOS
     C*
     C                   MOVEL(P)  V4DOS         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V4DOS
     C*
     C                   MOVEL(P)  CACODE        V4CODE
     C*** BEG CHG *** 03/28/02 ****************************************
     C******************** MOVELCAPMOD    V4MD
     C                   MOVEL     XXPMOD        V4MD
     C*** END CHG *** 03/28/02 ****************************************
     C*
     C                   MOVEL     CAEOB#        V4EOB#
     C                   MOVEL(P)  V4EOB#        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V4EOB#
     C*
     C                   MOVEL     CAFEIN        V4FEIN
     C                   MOVEL     CAMOD         V4MOD
     C                   MOVEL     SCSLI         V4SLI
     C*
     C                   MOVEL     VIEW4         SCDATA
     C                   ENDIF
     C*
     C                   MOVE      CAMRA         SCMRA
     C                   MOVE      CANDIS        SCNDIS
     C                   MOVE      CAOTHR        SCOTHR
     C                   MOVE      CAOTH2        SCOTH2
     C*
     C*** BEG DEL *** 02/03/02 ****************************************
     C******************** EXSR ERRCKM
     C*** END DEL *** 02/03/02 ****************************************
     C*** BEGIN ADD *** 11/19/01 **************************************
     C*
     C* DELETE THE OVERRIDE OF VARIOUS STATE SPECIC FILES.
     C*
     C     *IN16         IFEQ      *ON
     C                   EXSR      DELOVR
     C                   SETOFF                                       16
     C                   ENDIF
     C*** END   ADD *** 11/19/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Messages
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   CALL      'F0675R'
     C                   PARM      ' '           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      DFRRN5        ##RRN2
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OPT7 - Network Info
     C*================================================================
     C     OPT7          BEGSR
     C     STOP          IFEQ      *OFF
     C                   CALL      'N0582R'
     C                   PARM      OPTNM2        ##PRDS           40
     C                   PARM      *BLANKS       ##CALL
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCDSEQ
     C                   PARM                    ##RTCD
     C                   PARM                    KKCO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   ENDIF
     C     KHEND         ENDSR
     C*================================================================
     C* OPT8 - Work w/Amounts
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'F0397R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      SCDBR2        ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C                   PARM      Q1PGM         @@1PGM            6
     C*
     C                   ENDSR
     C*================================================================
     C* OPT11  - EOB/Notes
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   CALL      'F0356R'
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DFRRN5        ##RRN2            9 0
     C                   PARM      SCDBRR        ##RRN3            9 0
     C*
     C                   ENDSR
     C*** BEG ADD *** 05/01/03 ****************************************
     C*================================================================
     C* OPT12 - State rule
     C*================================================================
     C     OPT12         BEGSR
     C*
     C                   CALL      'F2400R01'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      DFRRN6        ##RRN             9 0
     C                   PARM      SCAST         ##ST              2
     C                   PARM      ##CO#         ##CO#             3 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT13 - Correct Coding rule
     C*================================================================
     C     OPT13         BEGSR
     C*
     C* USE RELATIVE RECORD NUMBER FROM API FA714R CALLED IN ERRCK1.
     C* CALL PROGRAM BASED ON FLAG IN FDCAP (CACCDF)
     C*
     C* CHAIN OUT TO FDCAP TO FIND WHICH WORK WITH TO CALL BASE ON
     C* FDCAP(CACCDF)
     C*
     C                   Z-ADD     SCDSEQ        KKDSEQ
     C*
     C     CAKY1         CHAIN(N)  PCA02                              99
     C*
     C* Convert date to pass in
     C*
     C                   SELECT
     C*
     C     CACCDF        WHENEQ    '1'
     C     CACCDF        OREQ      '2'
     C     CACCDF        OREQ      '5'
     C     CACCDF        OREQ      '6'
     C*
     C* COMPREHENSIVE AND COMPONENT CODE INFO (FMBNP)
     C*
     C                   CALL      'F2533R01'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      ##RRNC        ##RRN             9 0          FNBNP
     C                   PARM      DFRRN5        ##RRN4            9 0          FAEOP
     C                   PARM      CAFDAT        ##FDAT            7 0
     C*
     C* MUTUALLY EXCLUSIVE CODES (FMMUP)
     C*
     C     CACCDF        WHENEQ    '3'
     C     CACCDF        OREQ      '4'
     C     CACCDF        OREQ      '7'
     C     CACCDF        OREQ      '8'
     C*
     C                   CALL      'F2534R01'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      ##RRNC        ##RRN             9 0
     C                   PARM      DFRRN5        ##RRN4            9 0          FAEOP
     C                   PARM      CAFDAT        ##FDAT            7 0
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END ADD *** 05/01/03 ****************************************
     C*================================================================
     C* OPT21 - Detail Charge List
     C*================================================================
     C     OPT21         BEGSR
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   MOVE      SCMOD         ##MOD
     C*
     C                   CALL      'F9951R'
     C                   PARM      OPTNM         ##PRDS
     C                   PARM      '1'           ##CALL
     C                   PARM      KKSUB#        #2SUB#
     C                   PARM      KKFND         #2FND
     C                   PARM      CMSS#         ##SS#
     C                   PARM      CMACDT        ##ACDT
     C                   PARM      SCFEIN        #1FEIN
     C                   PARM                    ##MOD
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   PARM      'Y'           ##MODE
     C*** END   ADD *** 05/24/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=SEARCH
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '11'          OPT11
     C*** BEG ADD *** 05/01/03 ****************************************
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '13'          OPT13
     C*** END ADD *** 05/01/03 ****************************************
     C     SCSEL         CASEQ     '21'          OPT21
     C                   ENDCS
     C                   ENDSR
     C*** BEGIN ADD *** 11/19/01 **************************************
     C*================================================================
     C* OVRFIL - Overrides to various state specific files
     C*================================================================
     C     OVRFIL        BEGSR
     C*
     C                   Z-ADD     56            LENGTH           15 5
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(3)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(4)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(5)        QQCMD2
     C                   MOVEL(P)  SCAST         QQST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD2
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(6)        QQCMD2
     C                   MOVEL(P)  SCAST         QQST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD2
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(7)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*** END   ADD *** 11/19/01 **************************************
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEGIN ADD *** 02/26/02 **************************************
     C*
     C**  GET SYSTEM INFO
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
     C*** END   ADD *** 02/26/02 **************************************
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      @@CO#         #1CO#             3
     C                   MOVE      @@SUB#        #1SUB#            5
     C                   MOVE      @@FND         #1FND             3
     C                   MOVE      @@FYR         #1FYR             3
     C                   MOVE      @@CASE        #1CASE            7
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANKS       ##L1             78
     C     SCL2HD        PARM      *BLANKS       ##L2             78
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CXHMRA        SCHMRA
     C                   MOVE      ':'           SCHMRA
     C                   MOVEL(P)  CXHNET        SCHNET
     C                   MOVE      ':'           SCHNET
     C                   MOVEL(P)  CXHOT1        SCHOT1
     C                   MOVE      ':'           SCHOT1
     C                   MOVEL(P)  CXHOT2        SCHOT2
     C                   MOVE      ':'           SCHOT2
     C                   ENDIF
     C*
     C                   SETON                                        12
     C*** BEGIN ADD *** 11/19/01 **************************************
     C                   SETOFF                                       16
     C*** END   ADD *** 11/19/01 **************************************
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
     C   21KEY3A         READE     DBRCDFA                                95
     C   22KEY3A         READE     DBRCDFB                                95
     C   23KEY3A         READE     DBRCDF3                                95
     C   24KEY3A         READE     DBRCDF3                                95
     C*
     C     SSFEIN        WHENNE    *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C   21KEY4A         READE     DBRCDFC                                95
     C   22KEY4A         READE     DBRCDFD                                95
     C   23KEY4A         READE     DBRCDFE                                95
     C   24KEY4A         READE     DBRCDF4                                95
     C*
     C                   OTHER
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C   23KEY           READE     DBRCDF3                                95
     C   24KEY           READE     DBRCDF4                                95
     C                   ENDSL
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
     C   21KEY3A         READPE    DBRCDFA                                95
     C   22KEY3A         READPE    DBRCDFB                                95
     C   23KEY3A         READPE    DBRCDF3                                95
     C   24KEY3A         READPE    DBRCDF3                                95
     C*
     C     SSFEIN        WHENNE    *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C   21KEY4A         READPE    DBRCDFC                                95
     C   22KEY4A         READPE    DBRCDFD                                95
     C   23KEY4A         READPE    DBRCDFE                                95
     C   24KEY4A         READPE    DBRCDF4                                95
     C*
     C                   OTHER
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
     C   24KEY           READPE    DBRCDF4                                95
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C   21KEY1B         SETGT     DBRCDFA                            96
     C   22KEY2B         SETGT     DBRCDFB                            96
     C   23KEY3A         SETGT     DBRCDF3                            96
     C   24KEY3A         SETGT     DBRCDF3                            96
     C*
     C     SSFEIN        WHENNE    *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C   21KEY3B         SETGT     DBRCDFC                            96
     C   22KEY4B         SETGT     DBRCDFD                            96
     C   23KEY5B         SETGT     DBRCDFE                            96
     C   24KEY4A         SETGT     DBRCDF4                            96
     C*
     C                   OTHER
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C*
     C     SSEOB#        WHENNE    *ZEROS
     C   21KEY1B         SETLL     DBRCDFA                            96
     C   22KEY2B         SETLL     DBRCDFB                            96
     C   23KEY3A         SETLL     DBRCDF3                            96
     C   24KEY3A         SETLL     DBRCDF3                            96
     C*
     C     SSFEIN        WHENNE    *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C   21KEY3B         SETLL     DBRCDFC                            96
     C   22KEY4B         SETLL     DBRCDFD                            96
     C   23KEY5B         SETLL     DBRCDFE                            96
     C   24KEY4A         SETLL     DBRCDF4                            96
     C*
     C                   OTHER
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSEOB#        ANDNE     *ZEROS
     C     CAFEIN        ANDNE     SSFEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFEIN NE * ZEROS
     C*
     C                   MOVEL     SSMOD         XXMOD             3 0
     C     SSMOD         IFNE      *ZEROS
     C     SSEOB#        ANDNE     *ZEROS
     C     CAMOD         ANDNE     SSMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSMOD NE *ZEROS
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *OFF          MEETSS
     C                   ELSE
     C*** BEG ADD *** 01/07/03 ****************************************
     C                   MOVE      EAEOB#        KKEOB#
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *OFF          MEETSS
     C*** END ADD *** 01/07/03 ****************************************
     C*** BEG ADD *** 01/07/03 ****************************************
     C                   ENDIF
     C*** END ADD *** 01/07/03 ****************************************
     C                   ENDIF
     C*
     C     CAFLAG        IFNE      'P'
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt   Dt of Svc  Code        MD      EOMB#   FEIN/Mod       Charged       Paid
Opt  Code         MD      EOMB#   FEIN/Mod     Dt of Svc    Charged       Paid
Opt      EOMB#   FEIN/Mod     Dt of Svc  Code        MD     Charged       Paid
Opt    FEIN/Mod     Dt of Svc Code         MD      EOMB#    Charged       Paid
Opt   Dt of Svc  Code        MD      EOMB#   FEIN/Mod      Dt Entered
Opt  Code         MD      EOMB#   FEIN/Mod     Dt of Svc   Dt Entered
Opt      EOMB#   FEIN/Mod     Dt of Svc  Code        MD    Dt Entered
Opt    FEIN/Mod     Dt of Svc Code         MD      EOMB#   Dt Entered
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Messages               YYY
1077=Network info           YYY
1088=Work w/ Amounts        YYY
11111=EOB/Notes             YYY
11212=State rule            YYY
11313=Correct Coding rule   YYY
12121=Dtl Chrg List         YYY
203F3=Exit                  NNN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change Info           YNN
211F11=Change View          YNN
212F12=Cancel               YYY
214F14=DARTS                YYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
221F21=Dtl Chrg List        YYY
3
** HI-LITE MESSAGE
THERE ARE MESSAGES ASSOCIATED WITH THE HIGHLIGHTED LINE ITEMS
**  QCMDEXC TABLE
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  )
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FTABL)
DLTOVR FILE(FMPRL)
DLTOVR FILE(FTPSL)
DLTOVR FILE(FMSVP)
DLTOVR FILE(FMSVL1)
DLTOVR FILE(FMSVL2)
DLTOVR FILE(FMPRP)
