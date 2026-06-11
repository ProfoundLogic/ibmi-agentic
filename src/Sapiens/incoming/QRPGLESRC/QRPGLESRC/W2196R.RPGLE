     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
      *****************************************************************
      * REVISIONS:                                                    *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
      *                                                               *
      * 11/10/98  C:87  PROJECT: WC852                                *
      *                 Modify program to use WTCRL instead of WTABL. *
      *                 Add new input parameter for state.  Subset on *
      *                 state if passed into program.  Reformat screen*
      *                 to accomodate new fields.                     *
      *                                                               *
      *****************************************************************
     FW2196F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTCRP     IF   E             DISK
     F                                     RENAME(PCR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTABL   IF  E           K        DISK
     FWTCRL     IF   E           K DISK
     F***  END   CHG  ***  11/10/98  **********************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCR01:DBRCDF)
     F***  BEGIN ADD  ***  11/16/98  **********************************
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F***  END   ADD  ***  11/16/98  **********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Reason-
     D                                      Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2196R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D***  BEGIN ADD  ***  11/10/98  **********************************
     D CHGNUM          C                   CONST('SA020R')
     D*
     D*  Data structure for SCDATA
     D                 DS
     D  XXDATA                 1     68
     D  XXST                   1      2
     D  XXCODE                 7      8
     D  XXEFDT                11     20
     D  XXDESC                23     68

     I***  END   ADD  ***  11/10/98  **********************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@KEY
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   PARM                    @@ST              2
     C***  END   ADD  ***  11/10/98  **********************************
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
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C***  END   ADD  ***  11/10/98  **********************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** KEY2      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKKEY
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
     C***  END   CHG  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  11/16/98  **********************************
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C***  END   ADD  ***  11/16/98  **********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CRKEY         KKKEY
     C     *LIKE         DEFINE    CRKEY         @@KEY
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C***  END   ADD  ***  11/10/98  **********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C***  BEGIN ADD  ***  11/16/98  **********************************
     C     SSST          ORNE      *BLANKS
     C***  END   ADD  ***  11/16/98  **********************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/16/98  **********************************
     C     SSST          IFNE      *BLANKS
     C                   MOVEL(P)  SSST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        88  90
     C                   ENDIF                                                  SSST<>*BLANKS
     C***  END   ADD  ***  11/16/98  **********************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Subset on state if value passed in to program
     C     Q1PRMS        IFGE      5
     C     @@ST          ANDNE     *BLANK
     C                   MOVE      @@ST          SSST
     C                   MOVE      @@ST          KKST
     C                   ENDIF                                                  Q1PRMS>=5
     C***  END   ADD  ***  11/10/98  **********************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVE      *LOVAL        KKST
     C                   MOVE      *LOVAL        KKEFDT
     C***  END   ADD  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  11/16/98  **********************************
     C     SSST          IFNE      *BLANK
     C                   MOVE      SSST          KKST
     C                   ENDIF
     C***  END   ADD  ***  11/16/98  **********************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C*******************  MOVELSCPST1    KKKEY
     C                   MOVEL     SCPST1        KKST
     C                   MOVEL     SCPST9        KKKEY
     C                   MOVE      SCPS1B        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C***  END   CHG  ***  11/10/98  **********************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCKEY         KKKEY
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVEL     SCST          KKST
     C                   Z-ADD     SCEFDT        KKEFDT
     C***  END   ADD  ***  11/10/98  **********************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CRSTAT        SCSTAT
     C                   MOVEL     CRKEY         SCKEY
     C*
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C*******************  MOVELCRKEY     SCDATA    P
     C*******************  MOVE CRDESC    SCDATA
     C                   MOVEL     CRKEY         XXCODE
     C                   MOVEL     CRDESC        XXDESC
     C                   MOVE      CRST          XXST
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C*
     C                   Z-ADD     CREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEFDT
     C*
     C                   MOVE      CRST          SCST
     C                   MOVEL     CREFDT        SCEFDT
     C*
     C                   MOVEL(P)  XXDATA        SCDATA
     C***  END   CHG  ***  11/10/98  **********************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
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
     C*
     C                   MOVEL     SCKEY         @@KEY
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CRSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/16/98  **********************************
     C     SSST          IFNE      *BLANKS
     C     SSST          ANDNE     CRST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSST<>*BLANKS
     C***  END   ADD  ***  11/16/98  **********************************
     C*
     C                   ENDSR
** Column headings
State Code  Eff Date   Description                                    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
213F13=User parms           YYN
217F17=Subset               YYY
3
