     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7354R - WORK WITH EXTRACT SUBMISSION UNITS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7350R                                          *
     H*                                                               *
     H*  CALLS TO: W7354R02                                           *
     H*            W7357R                                             *
     H*            W7358R                                             *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 06/06/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/20/05  C:15  Project: WC591                               *
     H*                  Added option to update the unit total info.  *
     H*                                                               *
     H*  08/19/05  C:15  Project: WC591                               *
     H*                  Added option for resubmitting a specific     *
     H*                  policy and an option for correcting a Unit   *
     H*                  Total record.                                *
     H*                                                               *
     H*  07/18/05  C:15  Support: WC089/48126                         *
     H*                  Removed security level check for option 4.   *
     H*                                                               *
     H*  11/10/03  C:15  Support: WC046/39822                         *
     H*                  Added option to delete policies.             *
     H*                                                               *
     H*  07/30/00  C:87  PROJECT: WC591                               *
     H*                  Added state code to processing               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7354F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMSRP     IF   E             DISK
     F                                     RENAME(PSR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMSRL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSR01:DBRCDF)
     F*
     F*  Statistical submission file
     FWMSSP     IF   E             DISK
     F*
     F*** Begin add *** 08/19/05 **************************************
     FWMSRP3L   IF A E           K DISK
     F                                     RENAME(PSR01:PSR03)
     FWMSRP2L   UF A E           K DISK
     F                                     RENAME(PSR01:PSR02)
     FWMSQL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWMSQP2L   IF A E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PSQ01:PSQ02)
     F*** End add ***** 08/19/05 **************************************
     F*** Begin dlt *** 07/18/05 **************************************
     F*  Admin/darts control definition file
     F******WMEXL   IF  E           K        DISK
     F*** End dlt ***** 07/18/05 **************************************
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Extracted Submission-
     D                                      Units')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W7354R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structures for SCDATA
     D                 DS
     D  XXDATA                 1     73
     D  XXEMP#                 1      9
     D  XXFYR                 13     15
     D  XXNAME                18     55
     D  XXEFDT                59     68
     D  XXRPT#                72     73
     D*
     D*** Begin add *** 08/19/05 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** End add ***** 08/19/05 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
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
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*** Begin add *** 08/19/05 **************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** End add ***** 08/19/05 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   KFLD                    KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*
     C*** Begin add *** 08/19/05 **************************************
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCORR
     C*** End add ***** 08/19/05 **************************************
     C*
     C     SRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C*
     C*  Darts/admin control defn key
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Fund master file key
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SRCO#         KKCO#
     C     *LIKE         DEFINE    SRFND         KKFND
     C     *LIKE         DEFINE    SRRPMY        KKRPMY
     C     *LIKE         DEFINE    SREMP#        KKEMP#
     C     *LIKE         DEFINE    SRFYR         KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C     *LIKE         DEFINE    SRSTCD        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*** Begin add *** 08/19/05 **************************************
     C     *LIKE         DEFINE    SRCORR        KKCORR
     C*** End add ***** 08/19/05 **************************************
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
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '4'
     C*** Begin dlt *** 07/18/05 **************************************
     C* CHAIN TO WMEXP ( ADMIN DARTS FILE ) TO CHK SECURITY LEVEL
     C********** EXKY      CHAINPEX01                60
     C**********
     C********** ##LVL     IFLT EXDLTL                     NOT AUTHORIZED
     C********** *IN60     OREQ *ON                        NOT FOUND
     C**********           SETON                     77  90
     C**********           ENDIF                           ##LVL LT EXDLTL
     C*** End dlt ***** 07/18/05 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
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
     C     @@RRN         CHAIN     PSS01                              60
     C*
     C                   MOVE      SSCO#         KKCO#
     C                   MOVE      SSFND         KKFND
     C                   MOVE      SSRPMY        KKRPMY
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   MOVE      *LOVAL        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*
     C                   MOVE      SSFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     SSRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        KKEMP#
     C                   MOVE      SCPS1A        KKFYR
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCFYR         KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   MOVE      SCSTCD        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      SREMP#        SCEMP#
     C                   MOVE      SRFYR         SCFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   MOVE      SRSTCD        SCSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*
     C*  Edit stuff for output
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  SREMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEMP#
     C*
     C                   MOVEL(P)  SRFYR         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFYR
     C*
     C                   MOVEL     SRNAME        XXNAME
     C*
     C                   MOVE(P)   SRRPT#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVE      XXNUM         XXRPT#
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C                   Z-ADD     SREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEFDT
     C*
     C                   MOVEL     XXDATA        SCDATA
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT7 - Process option 7 (work with exposure records)
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   MOVE      @@CALL        ##MODE
     C*
     C                   CALL      'W7357R'      ACDPL
     C*
     C                   ENDSR                                                  OPT7
     C*================================================================
     C* OPT8 - Process option 8 (work with loss records)
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   MOVE      @@CALL        ##MODE
     C*
     C                   CALL      'W7358R'      ACDPL
     C*
     C                   ENDSR                                                  OPT8
     C*
     C*** Begin add *** 10/20/05 **************************************
     C*================================================================
     C* OPT9 - Process option 9 (Unit totals)
     C*================================================================
     C     OPT9          BEGSR
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE      SCSTCD        KKSTCD
     C     KEY1          CHAIN     PSQ01                              60
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7354R09'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN3        ##RRN
     C*
     C                   ENDSR                                                  OPT9
     C*** End add ***** 10/20/05 **************************************
     C*** Begin add *** 08/19/05 **************************************
     C*================================================================
     C* OPT11 - Process option 11 (resubmit policy)
     C*================================================================
     C     OPT11         BEGSR
     C*
     C*  Determine if a "resubmission" record already exists.
     C*  If not, create it.
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE      SCSTCD        KKSTCD
     C     KEY1          CHAIN     PSR03                              99
     C     *IN99         DOWEQ     *OFF
     C     SRSTAT        ANDEQ     'P'
     C     KEY1          READE     PSR03                                  99
     C                   ENDDO
     C*
     C*  Load the correction fields.
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   MOVE      ' '           SRSTAT
     C                   MOVE      Q1USER        SRUSER
     C                   MOVE      Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVE      SRUSER        SRIUSR
     C                   MOVE      SRDT          SRIDT
     C                   WRITE     PSR03
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR                                                  OPT11
     C*================================================================
     C* OPT12 - Process option 12 (Unit Total Corrections)
     C*================================================================
     C     OPT12         BEGSR
     C*
     C                   EXSR      WRTHDR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7354R22'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C*
     C                   ENDSR                                                  OPT12
     C*** End add ***** 08/19/05 **************************************
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
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C*** Begin add *** 10/20/05 **************************************
     C     SCSEL         CASEQ     '9'           OPT9
     C*** End add ***** 10/20/05 **************************************
     C*** Begin add *** 08/19/05 **************************************
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C*** End add ***** 08/19/05 **************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21SRKY          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21SRKY          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
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
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
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
     C*** Begin add *** 08/19/05 **************************************
     C*================================================================
     C* WRTHDR - This subroutine creates the header and unit total
     C*   correction records.
     C*================================================================
     C     WRTHDR        BEGSR
     C*
     C*  Determine if a header correction record already exists.
     C*  If not, create it.
     C*
     C                   Z-ADD     0             SVCORR            1 0
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE      SCSTCD        KKSTCD
     C     KEY1          CHAIN     PSR02                              99
     C     *IN99         DOWEQ     *OFF
     C     SRSTAT        ANDEQ     'P'
     C     KEY1          READE     PSR02                                  99
     C                   ENDDO
     C                   MOVE      SRCORR        SVCORR
     C*
     C*  Load the correction fields.
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   ADD       1             SVCORR
     C                   MOVE      SVCORR        SRCORR
     C                   MOVE      'T'           SRCTYP
     C                   MOVE      Q1USER        SRUSER
     C                   MOVE      Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVE      SRUSER        SRIUSR
     C                   MOVE      SRDT          SRIDT
     C                   WRITE     PSR02
     C*
     C     KEY1          CHAIN     PSQ01                              60
     C                   MOVE      SVCORR        SQCORR
     C                   MOVE      Q1USER        SQUSER
     C                   MOVE      Q1PGM         SQPGM
     C                   Z-ADD     SRTIME        SQTIME
     C                   Z-ADD     SRDT          SQDT
     C                   MOVE      SQUSER        SQIUSR
     C                   MOVE      SQDT          SQIDT
     C                   WRITE     PSQ02
     C*
     C                   ELSE
     C*
     C     SRCTYP        IFNE      'T'
     C                   MOVE      'M'           SRCTYP
     C                   ENDIF
     C                   UPDATE    PSR02
     C*
     C                   MOVE      SRCORR        KKCORR
     C     KEY2          CHAIN     PSQ02                              60
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C*** End add ***** 08/19/05 **************************************
** Column headings
Opt  Employer#   Yr  Name                                      Eff Date   Rpt#
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1077=Work with Exposure RecsYYY
1088=Work with Loss Recs    YYY
1099=Unit total             YYY
11111=Resubmit policy       YNN
11212=Unit Total Correction YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
