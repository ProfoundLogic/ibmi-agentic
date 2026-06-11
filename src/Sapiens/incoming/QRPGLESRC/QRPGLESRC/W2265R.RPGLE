     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  05/26/98   C:33  SEQ#: WC048/11819                          *
     H*                   CORRECT REPOSITION & PAGING                *
     H*                                                              *
     H*  04/23/98   C:33  SEQ#: WC082/11034                          *
     H*                   BE SURE THE CORRECT RRN IS BEING LOADED TO *
     H*                   SCDBRR                                     *
     H****************************************************************
     FW2265F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTOCP     IF   E             DISK
     F                                     RENAME(POC01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWTOCL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POC01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWTOCL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(POC01:DBRCDF2)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Occupational Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2265R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@KEY
     C                   PARM                    @@DESC
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDESC
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    OCKEY         KKKEY
     C     *LIKE         DEFINE    OCDESC        KKDESC
     C     *LIKE         DEFINE    OCKEY         @@KEY
     C     *LIKE         DEFINE    OCDESC        @@DESC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
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
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
     C                   MOVE      *LOVAL        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C***  BEGIN CHG  ***  05/26/98  *********************************
     C***21*****           MOVELSCPST1    KKDESC
     C***22*****           MOVELSCPST2    KKKEY
     C   21              MOVEL     SCPST1        KKKEY
     C   22              MOVEL     SCPST2        KKDESC
     C***  END   CHG  ***  05/26/98  *********************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C***  BEGIN CHG  ***  05/26/98  *********************************
     C***21*****           MOVELSCDESC    KKDESC
     C***22*****           MOVELSCKEY     KKKEY
     C   21              MOVEL     SCKEY         KKKEY
     C   22              MOVEL     SCDESC        KKDESC
     C***  END   CHG  ***  05/26/98  *********************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     OCSTAT        SCSTAT
     C                   MOVEL     OCKEY         SCKEY
     C                   MOVEL     OCKEY         XXKEY             4
     C                   MOVEL     OCDESC        SCDESC
     C*
     C***  BEGIN CHG  ***  05/26/98  *********************************
     C***22*****           MOVELXXKEY     SCDATA    P
     C***22*****           MOVE OCDESC    SCDATA
     C**********
     C***21*****           MOVELOCDESC    SCDATA    P
     C***21*****           MOVE XXKEY     SCDATA
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  XXKEY         SCDATA
     C                   MOVE      OCDESC        SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  OCDESC        SCDATA
     C                   MOVE      XXKEY         SCDATA
     C                   ENDSL
     C***  END   CHG  ***  05/26/98  *********************************
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
     C                   MOVEL     SCDESC        @@DESC
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
     C***  BEGIN ADD  ***  04/23/98  *********************************
     C                   Z-ADD     2             VIEW#
     C***  END   ADD  ***  04/23/98  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C***  BEGIN CHG  ***  04/23/98  *********************************
     C***21***** KEY1      READEDBRCDF2                  95
     C***22***** KEY1      READEDBRCDF                   95
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C***  END   CHG  ***  04/23/98  *********************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C***  BEGIN CHG  ***  04/23/98  *********************************
     C***21***** KEY1      REDPEDBRCDF2                  95
     C***22***** KEY1      REDPEDBRCDF                   95
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C***  END   CHG  ***  04/23/98  *********************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C***  BEGIN CHG  ***  04/23/98  *********************************
     C***21***** KEY3      SETGTDBRCDF2              96
     C***22***** KEY2      SETGTDBRCDF               96
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C***  END   CHG  ***  04/23/98  *********************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C***  BEGIN CHG  ***  04/23/98  *********************************
     C***21***** KEY3      SETLLDBRCDF2              96
     C***22***** KEY2      SETLLDBRCDF               96
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C***  END   CHG  ***  04/23/98  *********************************
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
     C     SSSTAT        ANDNE     OCSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
 Code  Description                     Sts
 Description                     Code  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
213F13=User parms           YYN
217F17=Subset               YYY
3
