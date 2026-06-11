     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0042R - WORK WITH AGENT PRODUCERS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                PRODUCERS ASSIGNED TO A PARTICULAR AGENT.  EACH*
     H*                PRODUCER CAN BE ACCESSED FOR UPDATE, DISPLAY OR*
     H*                SELECTION DEPENDING UPON THE CALLING PROGRAM.  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0040R   - WORK WITH AGENTS                                *
     H*  CALLS TO:                                                    *
     H*    W0042R02 - AGENT PRODUCER INFORMATION (OPT 2/4/5/20)       *
     H*    SC010R   - DARTS (OPT 14)                                  *
     H*                                                               *
     H*  CREATED: 12/01/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/29/02  C:132 PROJECT: WC1071                              *
     H*                  Change W0042F to display Option 20 on Second *
     H*                  option line which was hidden.                *
     H*                                                               *
     H*  03/26/02  C:132 PROJECT: WC1071                              *
     H*                  ADD Cancel Date to Subfile Display           *
     H*                                                               *
     H*  02/01/02  C:90  PROJECT: WC1071                              *
     H*                  Remove Opt4=Delete. Added an edit if user    *
     H*                  attempts to inactivate a producer that is    *
     H*                  currently assigned to an active Agent        *
     H*                  History record.                              *
     H*                                                               *
     H*  06/10/98  C:33  SEQ#: WA095/12092                            *
     H*                  CHANGE # OF RECORDS IN SUB-FILE PAGE.        *
     H*                                                               *
     H*  02/04/98  C:33  PROJECT: WC683                               *
     H*                  ADD OPTION 14-DARTS                          *
     H*****************************************************************
     FW0042F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMAPP     IF   E             DISK
     F                                     RENAME(PAP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMAPL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAP01:DBRCDF)
     FWMAPL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAP01:DBRCDF2)
     FSMCOL     IF   E           K DISK
     FWMAGP     IF   E             DISK
     F                                     RENAME(PAG01:PAG02)
      **** BEGIN ADD **** 02/01/02 ************************************
     FWMAGL     IF   E           K DISK
     FWMAAL     IF   E           K DISK
     FWMAHL6    IF   E           K DISK
      **** END   ADD **** 02/01/02 ************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D***  BEGIN CHG  ***  02/04/98  *********************************
     D**********          OF      1  13  1   OFD    35   Options, Function
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D***  END   CHG  ***  02/04/98  *********************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  VIEW1                  1     70
      **** BEGIN CHG **** 03/26/02 ************************************
     D*******                                 1  62 V1NAME
     D******                                 65  69 V1PROD
     D  V1NAME                 1     50
     D  V1PROD                53     57
     D  V1CXDT                60     69
      **** END CHG ****** 03/26/02 ************************************
     D                 DS
     D  VIEW2                  1     70
     D  V2PROD                 1      5
      **** BEGIN CHG **** 03/26/02 ************************************
     D*******                                 8  69 V2NAME
     D  V2NAME                 8     57
     D  V2CXDT                60     69
      **** END CHG ****** 03/26/02 ************************************
     D*
     D***  BEGIN ADD  ***  02/04/98  *********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D***  END   ADD  ***  02/04/98  *********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Producers')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0042R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D***  BEGIN CHG  ***  06/10/98  *********************************
     D**********    12                    C         SF1PAG
     D SF1PAG          C                   CONST(11)

     I***  END   CHG  ***  06/10/98  *********************************

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
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
     C                   PARM                    ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKPROD
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKPROD
     C*
      **** BEGIN ADD **** 02/01/02 ************************************
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    AGCO#
     C                   KFLD                    AGFEIN
     C                   KFLD                    AGMOD
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFND
     C                   KFLD                    AAAGT#
     C                   KFLD                    SCPROD
      **** END   ADD **** 02/01/02 ************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C***  END   ADD  ***  02/04/98  *********************************
     C     *LIKE         DEFINE    APCO#         KKCO#
     C     *LIKE         DEFINE    APFEIN        KKFEIN
     C     *LIKE         DEFINE    APMOD         KKMOD
     C     *LIKE         DEFINE    APPROD        KKPROD
     C     *LIKE         DEFINE    APLSN         KKLSN
     C     *LIKE         DEFINE    APFSN         KKFSN
     C     *LIKE         DEFINE    APLSN         U1PDSC
     C     *LIKE         DEFINE    APLSN         U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSLSN         IFNE      *BLANK
     C     SSSTAT        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSLSN<>*BLANK
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
      **** BEGIN ADD **** 02/01/02 ************************************
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '20'
     C     SCSTAT        ANDEQ     'A'
     C*
     C     AGKY1         CHAIN     WMAGL                              60
     C*
     C     AAKY          SETLL     PAA01
     C     AAKY          READE     PAA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*
     C     AHKY          SETLL     PAH01
     C     AHKY          READE     PAH01                                  61
     C     *IN61         DOWEQ     *OFF
     C*
     C     AHSTAT        IFNE      'I'
     C                   SETON                                        71  90
     C                   LEAVE
     C                   ENDIF                                                  AHSTAT <> I    CCKM
     C*
     C     AHKY          READE     PAH01                                  61
     C                   ENDDO                                                  *IN61
     C*
     C     AAKY          READE     PAA01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSL
     C*
      **** END   ADD **** 02/01/02 ************************************
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSLSN
     C                   CLEAR                   SSSTAT
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVE      'A'           SSSTAT
     C                   ENDIF                                                  @@CALL=2
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKPROD
     C                   CLEAR                   KKLSN
     C                   CLEAR                   KKFSN
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST2        KKLSN
     C                   MOVEL(P)  SCPSTA        KKFSN
     C                   CLEAR                   KKPROD
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCPST1        KKPROD
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCPROD        KKPROD
     C                   MOVEL(P)  SCLSN         KKLSN
     C                   MOVEL(P)  SCFSN         KKFSN
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     APPROD        SCPROD
     C                   MOVEL(P)  APLSN         SCLSN
     C                   MOVEL(P)  APFSN         SCFSN
     C                   MOVEL     APSTAT        SCSTAT
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL     APPROD        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1PROD
     C                   MOVEL     XXNUM         V2PROD
     C*
     C                   MOVEL(P)  APLSN         V2NAME
     C*
     C     APFSN         IFNE      *BLANK
     C                   CAT(P)    ',':0         V2NAME
     C                   CAT       APFSN:1       V2NAME
     C                   ENDIF                                                  APFSN
     C*
     C                   MOVEL(P)  V2NAME        V1NAME
     C*
      **** BEGIN CHG **** 03/26/02 ************************************
      * If have a Cancel Date for Producer, output in subfile
     C     APCXDT        IFNE      *ZEROS
     C                   Z-ADD     APCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNUM
     C                   MOVEL     'DATE'        EDTMSK
      *
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         V1CXDT
     C                   MOVEL     XXNUM         V2CXDT
      *
     C                   ELSE
     C                   MOVE      *BLANKS       V1CXDT
     C                   MOVE      *BLANKS       V2CXDT
     C                   ENDIF
      **** END CHG ****** 03/26/02 ************************************
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  VIEW1         SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  VIEW2         SCDATA
     C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*===============================================================
     C*
     C* OPTION 8=WORK WITH NON-RESIDENT STATES
     C*
     C     OPT8          BEGSR
     C*
     C  N09              MOVE      ' '           ##CALL
     C   09              MOVE      '1'           ##CALL
     C*
     C                   CALL      'W0043R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT8
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C*===============================================================
     C* OPT14 - SUBROUTINE FOR OPTION 14
     C*===============================================================
     C*
     C* OPTION 14=DARTS
     C*
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFEIN        QCFEIN
     C                   Z-ADD     SCMOD         QCMOD
     C*
     C*  CALL COMMENT PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0040R'      XXPGM
     C*
     C                   ENDSR                                                  OPT14
     C***  END   ADD  ***  02/04/98  *********************************
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '8'           OPT8
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     SCSEL         CASEQ     '14'          OPT14
     C***  END   ADD  ***  02/04/98  *********************************
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     @@RRN         CHAIN     WMAGP                              60
     C*
     C                   Z-ADD     AGCO#         KKCO#
     C                   Z-ADD     AGFEIN        KKFEIN
     C                   Z-ADD     AGMOD         KKMOD
     C*
     C                   Z-ADD     AGFEIN        SCFEIN
     C                   Z-ADD     AGMOD         SCMOD
     C                   MOVEL(P)  AGNAME        SCANAM
     C*
     C                   Z-ADD     @@RRN         ##RRN2
     C                   CLEAR                   @@RRN
     C*
     C     KKCO#         CHAIN     PCO01                              60
     C*
B02  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCCONM
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C*  CHECK LAST NAME AGAINST SEARCH STRING
     C*
B01  C     SSLSN         IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  APLSN         U1PDSC
     C     LO:UP         XLATE(P)  SSLSN         U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSLSN<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     APSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTAT<>*BLANK
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt Name                                                Prod#  Cancel Dte  Sts
Opt Prod#  Name                                                Cancel Dte  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1088=Non-resident states    YYY
11414=DARTS                 YYN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
