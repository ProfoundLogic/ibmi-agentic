     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0043R - WORK WITH PRODUCERS NON-RESIDENT STATES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL NON-   *
     H*                RESIDENT STATES ASSIGNED TO A PRODUCER.  EACH  *
     H*                STATE CAN BE ACCESSED FOR UPDATE, DISPLAY OR   *
     H*                SELECTION DEPENDING UPON THE CALLING PROGRAM.  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0042R   - WORK WITH PRODUCER NON-RESIDENT STATES          *
     H*  CALLS TO:                                                    *
     H*    W0043R02 - AGENT PRODUCER INFORMATION (OPT 2/4/5/20)       *
     H*                                                               *
     H*  CREATED: 12/11/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0043F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMPSP     IF   E             DISK
     F                                     RENAME(PPS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMPSL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPS01:DBRCDF)
     FWMAPP     IF   E             DISK
     FWMAGL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  VIEW1                  1     70
     D  V1ST                   1      2
     D  V1LFLG                 6      6
     D  V1LIC#                10     24
     D  V1RWDT                27     36
     D  V1EXDT                39     48
     D  V1LTYP                51     55
     D  V1STAT                59     59
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Producer Non-Residen-
     D                                     t States')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0043R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)

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
     C                   PARM      SCDBRR        ##RRN             9 0          NON-RESIDENT FILE
     C                   PARM                    ##RRN2            9 0          PRODUCER FILE
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
     C                   KFLD                    KKPROD
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKPROD
     C                   KFLD                    KKST
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    APCO#
     C                   KFLD                    APFEIN
     C                   KFLD                    APMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PSCO#         KKCO#
     C     *LIKE         DEFINE    PSFEIN        KKFEIN
     C     *LIKE         DEFINE    PSMOD         KKMOD
     C     *LIKE         DEFINE    PSPROD        KKPROD
     C     *LIKE         DEFINE    PSST          KKST
     C     *LIKE         DEFINE    SCDBRR        XXDBRR
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSSTAT<>*BLANK
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
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
     C                   Z-ADD     APCO#         KKCO#
     C                   Z-ADD     APFEIN        KKFEIN
     C                   Z-ADD     APMOD         KKMOD
     C                   Z-ADD     APPROD        KKPROD
     C                   CLEAR                   KKST
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKST
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCST          KKST
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  PSST          SCST
     C*
     C                   MOVEL(P)  PSST          V1ST
     C                   MOVE      PSLFLG        V1LFLG
     C                   MOVEL(P)  PSLIC#        V1LIC#
     C*
     C                   MOVEL(P)  'DATE'        EDTMSK
     C*
     C                   Z-ADD     PSRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   MOVEL(P)  XXDATE        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1RWDT
     C*
     C                   Z-ADD     PSEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVEL(P)  XXDATE        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1EXDT
     C*
     C                   MOVEL(P)  PSLTYP        V1LTYP
     C                   MOVE      PSSTAT        V1STAT
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  VIEW1         SCDATA
     C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
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
     C     @@RRN         CHAIN     WMAPP                              60
     C*
     C                   Z-ADD     APCO#         KKCO#
     C                   Z-ADD     APFEIN        KKFEIN
     C                   Z-ADD     APMOD         KKMOD
     C                   Z-ADD     APPROD        KKPROD
     C*
     C                   Z-ADD     APFEIN        SCFEIN
     C                   Z-ADD     APMOD         SCMOD
     C                   Z-ADD     APPROD        SCPROD
     C*
     C     APLSN         CAT(P)    ',':0         SCPNAM
     C     SCPNAM        CAT       APFSN:1       SCPNAM
     C*
     C     AGKY          CHAIN     WMAGL                              60
     C*
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
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     PSSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTAT<>*BLANK
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt  St  Lic  License #        Renewal Dt   Exp Date   Type   Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
