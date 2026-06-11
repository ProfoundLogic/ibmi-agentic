     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0584R1 - WORK WITH EOMB'S CHECKS
     H*****************************************************************
     H*  Created: 03/08/99  C:56 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H*  06/07/99 C:56  SUPPORT: MC025 #18359                         *
     H*                 Changed security level and type chack to use  *
     H*                 API SA006R.                                   *
     H*****************************************************************
     H*
     FW0584F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDPAP     IF   E             DISK
     F                                     RENAME(PPA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDPALG    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPA01:DBRCDF)
     FWMPDL     IF   E           K DISK
     F*  User Parameter File
     FSMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D DATA            S             74    DIM(1)
     D DTA1            S              1    DIM(74)                              VIEW 1 DATA
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB''s Checks')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W058XR02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D NOCHEC          C                   CONST('No checks for this -
     D                                     EOMB')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN1            9 0          faeop
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #ACO#             3
     C                   PARM                    #AFND             3
     C                   PARM                    #AFYR             3
     C                   PARM                    #ACASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*
     C     KEY11         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* key list for user parameter file
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSR
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PAEOB#        KKEOB#
     C     *LIKE         DEFINE    PAEOBS        KKEOBS
     C     *LIKE         DEFINE    PAFEIN        KKFEIN
     C     *LIKE         DEFINE    PAMOD         KKMOD
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
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
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C                   Z-ADD     0             KKEOBS
     C*
     C** GET WCMS FUND NUMBER
     C*
     C                   MOVEL     ##CO#         #1CO#
     C                   MOVEL     @@SUB#        #1SUB#
     C                   MOVEL     @@FND         #1FND
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C     ##RTCD        IFNE      'F'
     C                   MOVE      ##WCF#        KKFND
     C                   ENDIF                                                  ##RTCD NE F
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKEOBS
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      'Y'           SECCHK
     C                   SETOFF                                       31
     C                   CLEAR                   SCDATA
     C*
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXDATE           10
     C*
     C                   MOVE      PAEOBS        XXRCN             3
     C*
     C                   Z-ADD     PACHK#        SCCHK#            7 0
     C                   MOVEL(P)  SCCHK#        ##NUM            80
     C                   MOVE      *BLANKS       ##MASK           10
     C                   Z-ADD     0             ##DEC             3 0
     C                   MOVE      *BLANKS       ##THOU            1
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXCHK            12
     C                   MOVE      XXCHK         XXCHK#           10
     C*
     C     PAAMT         IFLT      *ZEROS
     C                   MOVE      '-'           XSIGN             1
     C                   ELSE
     C                   MOVE      *BLANKS       XSIGN
     C                   ENDIF                                                  RPAMT LT 0
     C*
     C                   MOVEL(P)  PAAMT         ##NUM            80
     C                   MOVEL(P)  'J'           ##MASK           10
     C                   Z-ADD     2             ##DEC             3 0
     C                   MOVE      *BLANKS       ##THOU            1
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXAMT1           12
     C                   MOVE      XXAMT1        XXAMT            14
     C*
     C                   MOVEL     PAFEIN        XXFEIN            9
     C                   MOVE      '-'           XXDASH            1
     C                   MOVEL     PAMOD         XXMOD             3
     C*
     C                   Z-ADD     PAFEIN        KKFEIN
     C                   Z-ADD     PAMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        XXNAME           10
     C                   ELSE
     C                   MOVE      *BLANKS       XXNAME
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C*** LOAD VIEW# 1 FIELDS
     C*
     C                   SELECT
     C*
     C     HD#           WHENEQ    1
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     XXRCN         DTA1(Y)
     C                   ADD       5             Y
     C                   MOVEA     XXAMT         DTA1(Y)
     C                   ADD       14            Y
     C                   MOVEA     XSIGN         DTA1(Y)
     C                   ADD       2             Y
     C                   MOVEA     XXDATE        DTA1(Y)
     C                   ADD       16            Y
     C                   MOVEA     XXCHK#        DTA1(Y)
     C                   ADD       7             Y
     C                   MOVEA     XXFEIN        DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     XXDASH        DTA1(Y)
     C                   ADD       2             Y
     C                   MOVEA     XXMOD         DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     XXNAME        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - PAYMENT CORRECTION
     C*================================================================
     C     OPT6          BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   MOVE      PACO#         #1CO#
     C                   MOVE      PAFND         #1FND
     C                   MOVE      PAFYR         #1FYR
     C                   MOVE      PACASE        #1CASE
     C                   Z-ADD     0             ##RRN
     C*
     C* GET WMCMP RELATIVE RECORD NUMBER
     C*
     C                   CALL      'WA004R'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM                    ##RRN
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   CALL      'W0120R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      ' '           ##MODE
     C                   PARM                    ##RRN
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OPT21 - Check Info
     C*================================================================
     C     OPT21         BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   MOVE      PACO#         #1CO#
     C                   MOVE      PAFND         #1FND
     C                   MOVE      PAFYR         #1FYR
     C                   MOVE      PACASE        #1CASE
     C                   MOVE      PAEOB#        #1EOB#            9
     C                   CALL      'W0070R50'                           99
     C                   PARM      ' '           ##KC
     C     *INKL         PARM      ' '           ##KL
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM      ' '           @@CALL
     C                   PARM                    #1EOB#
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '21'          OPT21
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           @@KC
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      '1'           @@KL
     C                   ENDIF
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         ANDEQ     *OFF
     C*
     C*
     C* VALIDATE SECURITY IF 'SECCHK' FLAG IS SET TO 'Y'
     C*
     C     SECCHK        IFEQ      'Y'
     C                   MOVE      @@FND         ##FND
     C                   MOVE      @@SUB#        ##SUB#
     C                   CALL      'FA715R'
P01  C                   PARM      ' '           ##RTCD            1
P02  C                   PARM                    ##CO#
P03  C                   PARM                    ##SUB#            5 0
P04  C                   PARM                    ##FND
P05  C                   PARM                    ##PVMC            1 0          Init Med
P06  C                   PARM                    ##PVPR            1 0          Init Prov
P07  C                   PARM                    ##WCUP            1            Allw Upd WCMS Clms
P08  C                   PARM                    ##CLDL            3 0          Clm Dlt Lvl
P09  C                   PARM                    ##CLDT            1            Clm Dlt Typ
P10  C                   PARM                    ##CLTL            3 0          Clm Xfer Lvl
P11  C                   PARM                    ##CLTT            1            Clm Xfer Typ
P12  C                   PARM                    ##SDCL            3 0          Settle Dt Lvl
P13  C                   PARM                    ##SDCT            1            Settle Dt Typ
P14  C                   PARM                    ##BSCL            3 0          BWC Sts Lvl
P15  C                   PARM                    ##BSCT            1            BWC Sts Typ
P16  C                   PARM                    ##DCDL            3 0          Del Diag Cd Lvl
P17  C                   PARM                    ##DCDT            1            Del Diag Cd Typ
P18  C                   PARM                    ##DCCL            3 0          Chg Diag St Lvl
P19  C                   PARM                    ##DCCT            1            Chg Diag St Typ
P20  C                   PARM                    ##C#CL            3 0          Clt Case Lvl
P21  C                   PARM                    ##C#CT            1            Clt Case Typ
P22  C                   PARM                    ##FIMR            5            RMCD: From IMR
P23  C                   PARM                    ##TIMR            5            RMCD: To IMR
P24  C                   PARM                    ##FRN             5            RMCD: From RN
P25  C                   PARM                    ##TRN             5            RMCD: To RN
P26  C                   PARM                    ##CS#L            3 0          St Clm#  Lvl
P27  C                   PARM                    ##CS#T            1            St Clm#  Typ
P28  C                   PARM                    ##DNSL            3 0          Ovr DNYD Lvl
P29  C                   PARM                    ##DNST            1            Ovr DNYD Typ
P30  C                   PARM                    ##NERC            5            RMCD: New Emplr
P31  C                   PARM                    ##MEUS           10            New Employer
P32  C                   PARM                    ##NWPA            5            RMCD: ADD PC
P33  C                   PARM                    ##CPAA            5            RMCD: 'A' OR 'N'
P34  C                   PARM                    ##CPAD            5            RMCD: 'D'
P35  C                   PARM                    ##PLCL            3 0          LIAB CHG LVL
P36  C                   PARM                    ##PLCT            1            LIAB CHG TYP
P37  C                   PARM                    ##CKDL            3 0          EOMB DEL W/CHK LVL
P38  C                   PARM                    ##CKDT            1            EOMB DEL W/CHK TYP
     C*
     C                   MOVEL(P)  Q1USER        KKUSR
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C*** BEG CHG *** 06/07/99 ****************************************
     C********** UPLVL     IFLT ##CKDL
     C********** UPTYPE    ORLT ##CKDT
     C**********           SETON                     9071
     C**********           MOVE '1'       RSTPGM
     C**********           GOTO ENDEND
     C**********           ENDIF                           UPLBL < ##CKDL
     C*
     C                   Z-ADD     UPLVL         ##ULVL            3 0
     C                   MOVE      UPTYPE        ##UTYP            1
     C                   Z-ADD     ##CKDL        ##FLVL            3 0
     C                   MOVE      ##CKDT        ##FTYP            1
     C                   CALL      'SA006R'
     C                   PARM                    ##ULVL
     C                   PARM                    ##UTYP
     C                   PARM                    ##FLVL
     C                   PARM                    ##FTYP
     C                   PARM      ' '           ##RTCD
     C     ##RTCD        IFEQ      'F'
     C                   SETON                                        9071
     C                   MOVE      '1'           RSTPGM
     C                   GOTO      ENDEND
     C                   ENDIF                                                  ##RTCD = 'F'
     C*** END CHG *** 06/07/99 ****************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  SECCHK = 'Y'
     C                   ENDIF
     C*
     C     ENDEND        ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      @@SUB#        #1SUB#
     C                   MOVE      @@FND         #1FND
     C                   MOVE      @@FYR         #1FYR
     C                   MOVE      @@CASE        #1CASE
     C                   Z-ADD     @@EOB#        SCEOMB
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      ##CO#         #ACO#
     C                   MOVE      @@FND         #AFND
     C                   MOVE      @@FYR         #AFYR
     C                   MOVE      @@CASE        #ACASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   RETURN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C* GET HEADING LINES
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN1                         faeop
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C                   SETON                                        12
     C*
     C                   MOVEL(P)  NOCHEC        SCMESG
     C                   SETON                                        31
     C*
     C* INITIALIZE SELECT PROGRAM VARIABLES
     C*
     C                   MOVE      *BLANKS       SECCHK            1
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY11         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY11         READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   ENDSR
** Column headings
Opt  RCN#         Amount    Post Dt      Check#  Payee
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                NNN
1066=Payment Correction     NYN
12121=Check Info            NYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NYY
217F17=Subset               NNY
3
