     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H* 10/21/15 C:267 Support: 37326                                 *
     H*                Division restriction should be dependent on    *
     H*                employer restriction.                          *
     H*                                                               *
     H* 09/22/04 C:15  Support: WC114/43925                           *
     H*                Added an edit to require the group # entry if  *
     H*                the restriction flag is 'Y'.                   *
     H*                                                               *
     H* 07/29/04 C:132 PROJECT: GO099  43342                          *
     H*                Add code for processing F03 and F12 in PGMSTR  *
     H*                and in PGMEND before call next program.        *
     H*                                                               *
     H* 05/13/04 C:053 PROJECT: WC1186                                *
     H*                Add total incurred flag to calc rsrv amounts   *
     H*                                                               *
     H*  02/02/04  C:15  Support: WC081/40989                         *
     H*                  Implemented 81WMR/81WEU to base to allow     *
     H*                  Clt081 to enter users without issues to the  *
     H*                  reserve authority (WC1122).                  *
     H*                                                               *
     H* 04/01/03 C:053 PROJECT: WC1122                                *
     H*                Add changes to reserve authority limits        *
     H*                                                               *
     H* 11/13/02 C:062 PROJECT: SY078                                 *
     H*                Add Office location processing.                *
     H*                                                               *
     H* 11/12/02 C:136 PROJECT: WC577                                 *
     H*                Access multi-group restriction flag in SSYPF   *
     H*                for displaying F8=Restrictions or not.         *
     H*                                                               *
     H* 08/24/00  C:35   PROJECT: SUPPORT OP051 #24111                *
     H*                  GROUP NUMBER WAS BEING CLEARED IF F12 FROM   *
     H*                  GROUP SEARCH.                                *
     H*                                                               *
     H*  05/16/00 C:33 SEQ#: OP082/21740                              *
     H*                ONLY UPDATE UPFL5 BASED ON SCDIV               *
     H*
     H*  05/15/00 C:118 PROJECT: WC577
     H*                 Make display option chgs to display file
     H*                 - HI fields in display mode
     H*                 - Fed ID - UL in chg, PR and HI is display
     H*
     H*  11/17/99 C:90 PROJECT: 96NCP
     H*                ADD NEW OPTIONAL FIELD 'FEDERAL ID#' WITH
     H*                SEARCH CAPABILITY.
     H*
     H*  06/02/99 C:61 PROJECT: 101AT
     H*                Add code for F8=Restrictions.  In order to
     H*                use, need to add F8=Restrictions to display
     H*                file W0004R02/SCR01.
     H*
     H*  05/28/99 C:53 MC145
     H*                ADD WCMS, WCMC AND DARTS TO WORKFLOW
     H*
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*               -CHANGE SCDIV FROM NUMERIC TO ALPHA (NUMBERS    *
     H*                ONLY) TO ALLOW ALL DIVISIONS OR ONE DIVISION TO*
     H*                BE RESTRICTED.                                 *
     H*               -DIVISION RESTRICTION WILL BE LOADED TO UPFL5   *
     H*                (BEHIND THE SCENES).                           *
     H*                                                               *
     H*  03/18/99 C:33 PROJECT: 48WOM                                 *
     H*                ONLY SEARCH FOR EMPLOYERS W/IN THE GROUP ON THE*
     H*                SCREEN                                         *
     H*                                                               *
     H*  01/09/98  C:69  Changed program and srceen so default group
     H*                  is no longer required.
     H****************************************************************
     FW0004F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F*
     FWMEML     IF   E           K DISK
     F*
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      **** BEGIN ADD **** 11/17/99 ************************************
     FWMAAP     IF   E             DISK
     F*
     FWMAGP     IF   E             DISK
      **** END   ADD **** 11/17/99 ************************************
     F*  Physical file definition (for maintaining record)
     FSMUPP     UF A E             DISK
     F                                     RENAME(PUP01:DBRCDF)
     F*** BEGIN ADD *** 11/13/02 **************************************
     FWMOFL     IF   E           K DISK
     F*** END   ADD *** 11/13/02 **************************************
     F*** BEG ADD *** 04/01/03 **************************************
     FWMUPL     UF A E           K DISK
      /COPY SCOPYMT01D
     D*** END ADD *** 04/01/03 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***  BEGIN ADD  ***  05/10/99  *********************************
     D CHGNUM          C                   CONST('SA020R')
     D*
     D                 DS
     D  SCDIV                  1      5
     D  XXDIV1                 1      1
     D  XXDIV2                 2      2
     D  XXDIV3                 3      3
     D  XXDIV4                 4      4
     D  XXDIV5                 5      5
     D***  END   ADD  ***  05/10/99  *********************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMS User Parameters')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C*** BEG ADD *** 05/28/99 *************
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      @@RRN         ##RRN
     C*** END ADD *** 05/28/99 *************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           KFLD           SCDIV
     C                   KFLD                    XXDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C***  END   CHG  ***  05/10/99  *********************************
     C*** BEGIN ADD *** 11/13/02 **************************************
     C     OFKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCOFF#
     C*** END   ADD *** 11/13/02 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UPUSR         ##USR
     C     *LIKE         DEFINE    UPUSNM        ##USNM
     C     *LIKE         DEFINE    UPOUTQ        ##OUTQ
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    SCDIV         SVDIV
     C     *LIKE         DEFINE    SCEMP#        SVEMP#
     C     *LIKE         DEFINE    SCXEMP        SVXEMP
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*** BEG DEL *** 04/01/03 ****************************
     C*********            EXSR NXTPGM                     ADD 05/28/99
     C*** END DEL *** 04/01/03 ****************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFND         ANDNE     *ZEROS
     C     FNKEY         CHAIN     PFN01                              90
     C     *IN90         IFEQ      *ON
     C                   SETON                                        71
     C                   ELSE
     C     FNSTAT        IFEQ      'I'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCXFND        ANDNE     'Y'
     C     SCXFND        ANDNE     'N'
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C*** Begin add *** 09/22/04 **************************************
     C     SCXFND        IFEQ      'Y'
     C     SCFND         ANDEQ     *ZEROS
     C                   SETON                                        7190
     C                   ENDIF
     C*** End add ***** 09/22/04 **************************************
     C*
     C*  These edits are only preformed of the user profile exists
     C*  on the AS/400.
     C*
     C     *IN30         IFEQ      *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMP#        IFNE      *ZEROS
     C     SCXEMP        OREQ      'Y'
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     SCDIV         IFNE      *BLANKS
     C*
     C*  BE TO LOAD THE CORRECT VALUE IF FIELD EXIT WAS NOT PRESSSED
     C*
     C     XXDIV5        DOWEQ     *BLANK
     C                   MOVE      XXDIV4        XXDIV5
     C                   MOVE      XXDIV3        XXDIV4
     C                   MOVE      XXDIV2        XXDIV3
     C                   MOVE      XXDIV1        XXDIV2
     C                   CLEAR                   XXDIV1
     C                   ENDDO                                                  XXDIV5=*BLANKS
     C*
     C                   MOVE      SCDIV         XXDIV
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     EMKEY         CHAIN     PEM01                              90
     C*
     C***  END   ADD  ***  05/10/99  *********************************
     C                   ELSE
     C*
     C     EMKY2         CHAIN     PEM01                              90
     C*
     C                   ENDIF                                                  SCDIV<>*BLANKS
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     *IN90         IFEQ      *ON
     C                   SETON                                        76
     C***  BEGIN DEL  ***  02/02/04  **********************************
     C**********           ELSE
     C********** EMSTAT    IFEQ 'I'
     C**********           SETON                     7690
     C**********           ENDIF
     C***  END   DEL  ***  02/02/04  **********************************
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C  N90SCEMP#        IFEQ      *ZEROS
     C     SCDIV         ANDNE     *BLANKS
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCEMP#=*ZEROS
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   ENDIF
     C                   ENDIF
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C*
     C*  CHECK FOR DIV=*BLANK FIRST TIME THRU AND SEND MESSAGE
     C*
     C  N90SCEMP#        IFNE      *ZEROS
     C     SCDIV         ANDEQ     *BLANKS
     C     SCXEMP        ANDEQ     'Y'
     C*
     C     SCDIV         IFNE      SVDIV
     C     SCEMP#        ORNE      SVEMP#
     C     SCXEMP        ORNE      SVXEMP
     C                   SETON                                        89  90
     C                   MOVE      SCDIV         SVDIV
     C                   MOVE      SCEMP#        SVEMP#
     C                   MOVE      SCXEMP        SVXEMP
     C                   ENDIF                                                  SCDIV<>SVDIV
     C*
     C                   ENDIF                                                  SCEMP#<>*ZEROS
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCXEMP        ANDNE     'Y'
     C     SCXEMP        ANDNE     'N'
     C                   SETON                                        7890
     C                   ENDIF
     C***** BEG ADD ******* 06/02/99 **********************************
     C*
     C* Check for active restriction records
     C*
     C  N90SCXFND        IFEQ      'N'
     C     SCXEMP        ANDEQ     'N'
     C*
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      SCUSR         #@USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C     ##RCDS        IFEQ      *ON
     C                   SETON                                        88  90
     C                   ENDIF                                                  ##RCDS=*ON
     C*
     C                   ENDIF                                                  SCXFND=N
     C*
     C***** END ADD ******* 06/02/99 **********************************
     C*
     C*** BEG DEL *** 04/01/03 **********************************
     C******     *IN90     IFEQ *OFF
     C******     SCRAL     ANDLT*ZERO
     C******               SETON                     7290
     C******               ENDIF
     C******
     C******     *IN90     IFEQ *OFF
     C******     SCPAL     ANDLT*ZERO
     C******               SETON                     7390
     C******               ENDIF
     C*** END DEL *** 04/01/03 **********************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFL2         ANDNE     'Y'
     C     SCFL2         ANDNE     'N'
     C                   SETON                                        7490
     C                   ENDIF
     C*** BEGIN ADD *** 11/13/02 **************************************
     C*
     C* OFFICE LOCATION MUST BE VALID IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOFF#        ANDNE     *BLANKS
     C     OFKY          CHAIN     POF01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9079
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 11/13/02 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*** BEG ADD *** 04/01/03 **********************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPAL         ANDLT     *ZERO
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMRAL        ANDLT     *ZERO
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRAL        ANDLT     *ZERO
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCERAL        ANDLT     *ZERO
     C                   SETON                                        7590
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLRAL        ANDLT     *ZERO
     C                   SETON                                        7690
     C                   ENDIF
     C*
     C* BOTH ENTRIES CANNOT BE MADE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMRAL        IFGT      *ZERO
     C     SCRAL         ANDGT     *ZERO
     C     SCCRAL        ORGT      *ZERO
     C     SCRAL         ANDGT     *ZERO
     C     SCERAL        ORGT      *ZERO
     C     SCRAL         ANDGT     *ZERO
     C     SCLRAL        ORGT      *ZERO
     C     SCRAL         ANDGT     *ZERO
     C                   SETON                                        8190
     C                   ENDIF
     C                   ENDIF
     C*
     C* AT LEAST ONE ENTRY MUST BE MADE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMRAL        IFLE      *ZERO
     C     SCCRAL        ANDLE     *ZERO
     C     SCERAL        ANDLE     *ZERO
     C     SCLRAL        ANDLE     *ZERO
     C     SCRAL         ANDLE     *ZERO
     C*******              SETON                     8390
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRAL         ANDLT     *ZERO
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C* IF CUMM ENTRY MADE, MUST HAVE RESERVE TYPE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSV1        ANDEQ     *BLANKS
     C     SCRSV2        ANDEQ     *BLANKS
     C     SCRSV3        ANDEQ     *BLANKS
     C     SCRSV4        ANDEQ     *BLANKS
     C     SCRAL         ANDGT     *ZEROS
     C                   SETON                                        8290
     C                   ENDIF
     C*
     C* RESERVE TYPES MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSV1        ANDNE     *BLANKS
     C     SCRSV1        ANDNE     'M'
     C     SCRSV1        ANDNE     'C'
     C     SCRSV1        ANDNE     'E'
     C     SCRSV1        ANDNE     'L'
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSV2        ANDNE     *BLANKS
     C     SCRSV2        ANDNE     'M'
     C     SCRSV2        ANDNE     'C'
     C     SCRSV2        ANDNE     'E'
     C     SCRSV2        ANDNE     'L'
     C                   SETON                                        7890
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSV3        ANDNE     *BLANKS
     C     SCRSV3        ANDNE     'M'
     C     SCRSV3        ANDNE     'C'
     C     SCRSV3        ANDNE     'E'
     C     SCRSV3        ANDNE     'L'
     C                   SETON                                        7990
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSV4        ANDNE     *BLANKS
     C     SCRSV4        ANDNE     'M'
     C     SCRSV4        ANDNE     'C'
     C     SCRSV4        ANDNE     'E'
     C     SCRSV4        ANDNE     'L'
     C                   SETON                                        8090
     C                   ENDIF
     C*
     C*** END ADD *** 04/01/03 **********************************
     C*** BEG ADD *** 05/13/04 **********************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTINC        ANDNE     *BLANKS
     C     SCTINC        ANDNE     'Y'
     C                   SETON                                        8490
     C                   ENDIF
     C*** END ADD *** 05/13/04 **********************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*
     C*
     C*
     C*================================================================
     C* GETUSP - Get user profile information
     C*================================================================
     C     GETUSP        BEGSR
     C                   CALL      'SRRTVUSR'                           99
     C                   PARM                    ##USR
     C                   PARM      *BLANKS       ##USNM
     C                   PARM      *BLANKS       ##OUTQ
     C     *IN30         PARM                    ##ERR             1
     C                   ENDSR
     C***** BEG ADD ******* 06/02/99 **********************************
     C*
     C*
     C*
     C*================================================================
     C* KHDO   - Processing for F8=RESTRICTIONS
     C*================================================================
     C*
     C     KHDO          BEGSR
     C*
     C     SCXFND        IFNE      'Y'
     C     SCXEMP        ANDNE     'Y'
      *** BEG CHG *** 11/12/02 *************************************************
     C******************** SETON                     89  90
     C                   SETON                                        44  90
      *** NDG CHG *** 11/12/02 *************************************************
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      *ON
     C                   MOVE      '1'           ##CALL
     C*
     C                   ELSE
     C*
     C                   MOVE      ' '           ##CALL
     C                   ENDIF                                                  *IN09=*ON
     C*
     C                   CALL      'S0009R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      'W'           ##APPL            1
     C                   PARM                    ##USR
     C*
     C                   ENDIF                                                  SCXFND<>Y<>SCXEMP
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   ENDIF                                                  *INKC=*ON
     C*
     C                   ENDSR                                                  KHDO
     C*
     C***** END ADD ******* 06/02/99 **********************************
     C*
     C*
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     UPUSR         IFNE      *BLANKS
     C                   MOVEL     UPUSR         ##USR
     C                   EXSR      GETUSP
     C                   ENDIF
     C*
     C     UPUSR         CHAIN(N)  PUP01                              98        ADD 04/01/03
     C*
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   MOVEL     UPFYR         SCFYR
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     UPXFND        SCXFND
     C                   MOVEL     UPEMP#        SCEMP#
      **** BEGIN ADD **** 11/17/99 ************************************
     C                   Z-ADD     UPFEIN        SCFEIN
      **** END   ADD **** 11/17/99 ************************************
     C***  BEGIN DEL  ***  05/10/99  *********************************
     C**********           MOVELUPDIV     SCDIV
     C***  END   DEL  ***  05/10/99  *********************************
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   Z-ADD     SCEMP#        SVEMP#
     C*
     C     UPFL5         IFEQ      'Y'
     C***  BEGIN DEL  ***  05/16/00  *********************************
     C********** UPXEMP    ORNE 'Y'
     C***  END   DEL  ***  05/16/00  *********************************
     C*
     C                   MOVEL(P)  UPDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     XXNUM         SCDIV
     C*
     C     UPDIV         IFEQ      *ZERO
     C                   MOVE      '0'           SCDIV
     C                   ENDIF                                                  UPDIV=*ZERO
     C*
     C                   ELSE
     C*
     C                   CLEAR                   SCDIV
     C*
     C                   ENDIF                                                  UPFL5=Y
     C*
     C                   MOVEL     SCDIV         SVDIV
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   MOVEL     UPXEMP        SCXEMP
     C                   MOVEL     UPRAL         SCRAL
     C                   MOVEL     UPPAL         SCPAL
     C                   MOVEL     UPFL2         SCFL2
     C*** BEG ADD *** 04/01/03 **********************************
     C                   MOVEL     UPMRAL        SCMRAL
     C                   MOVEL     UPCRAL        SCCRAL
     C                   MOVEL     UPERAL        SCERAL
     C                   MOVEL     UPLRAL        SCLRAL
     C                   MOVE      UPRSV1        SCRSV1
     C                   MOVE      UPRSV2        SCRSV2
     C                   MOVE      UPRSV3        SCRSV3
     C                   MOVE      UPRSV4        SCRSV4
     C*** END ADD *** 04/01/03 **********************************
     C*** BEG ADD *** 05/13/04 **********************************
     C                   MOVE      UPTINC        SCTINC
     C*** END ADD *** 05/13/04 **********************************
     C*
     C     SCXFND        IFEQ      *BLANKS
     C                   MOVEL     'N'           SCXFND
     C                   ENDIF
     C*
     C     SCXEMP        IFEQ      *BLANKS
     C                   MOVEL     'N'           SCXEMP
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   MOVEL     SCXEMP        SVXEMP
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     SCFL2         IFEQ      *BLANKS
     C                   MOVEL     'N'           SCFL2
     C                   ENDIF
     C*** BEGIN ADD *** 11/13/02 **************************************
     C                   MOVEL(P)  UPOFF#        SCOFF#
     C                   MOVEL     *BLANKS       SCOFFN
     C*
     C     OFKY          CHAIN     POF01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCOFFN
     C                   ENDIF
     C*** END   ADD *** 11/13/02 **************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCFYR         UPFYR
     C                   MOVEL     SCFND         UPFND
     C                   MOVEL     SCXFND        UPXFND
     C                   MOVEL     SCEMP#        UPEMP#
     C                   MOVEL     SCDIV         UPDIV
     C                   MOVEL     SCXEMP        UPXEMP
     C                   MOVEL     SCRAL         UPRAL
     C                   MOVEL     SCPAL         UPPAL
     C                   MOVEL     SCFL2         UPFL2
      **** BEGIN ADD **** 11/17/99 ************************************
     C                   Z-ADD     SCFEIN        UPFEIN
      **** END   ADD **** 11/17/99 ************************************
     C*** BEGIN ADD *** 11/13/02 **************************************
     C                   MOVEL(P)  SCOFF#        UPOFF#
     C*** END   ADD *** 11/13/02 **************************************
     C*** BEG ADD *** 04/01/03 **********************************
     C                   MOVEL     SCMRAL        UPMRAL
     C                   MOVEL     SCCRAL        UPCRAL
     C                   MOVEL     SCERAL        UPERAL
     C                   MOVEL     SCLRAL        UPLRAL
     C                   MOVE      SCRSV1        UPRSV1
     C                   MOVE      SCRSV2        UPRSV2
     C                   MOVE      SCRSV3        UPRSV3
     C                   MOVE      SCRSV4        UPRSV4
     C*** END ADD *** 04/01/03 **********************************
     C*** BEG ADD *** 05/13/04 **********************************
     C                   MOVE      SCTINC        UPTINC
     C*** END ADD *** 05/13/04 **********************************
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     SCDIV         IFNE      *BLANK
     C***  BEGIN ADD  ***  10/21/15  *********************************
     C     SCXEMP        ANDEQ     'Y'
     C***  END   ADD  ***  10/21/15  *********************************
     C***  BEGIN DEL  ***  05/16/00  *********************************
     C********** SCXEMP    ANDEQ'Y'
     C***  END   DEL  ***  05/16/00  *********************************
     C                   MOVE      'Y'           UPFL5
     C                   ELSE
     C                   CLEAR                   UPFL5
     C                   ENDIF                                                  SCDIV<>*BLANK
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 05/28/99 *************************
     C*================================================================
     C* NXTPGM - Processing for other PROGRAMS
     C*================================================================
     C     NXTPGM        BEGSR
     C*
     C                   MOVE      @@MODE        ##MODE            1
     C*
     C     ##MCIF        IFEQ      'Y'
     C                   CALL      'SRCHKPGM'
     C                   PARM      'S0005R02'    ##PGM            10
     C                   PARM      '0'           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      '1'
     C                   CALL      'F0004R02'    ACDPL                  99
     C                   ENDIF
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *INKL
     C                   EXSR      GETDTM
     C                   ENDIF
     C                   ENDIF
     C*
     C     ENDNXT        ENDSR
     C*** END ADD *** 05/28/99 *************************
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C***** BEG ADD ******* 06/02/99 **********************************
     C*
     C     *INKH         CASEQ     *ON           KHDO                     90
     C                   ENDCS
     C*
     C***** END ADD ******* 06/02/99 **********************************
     C*
     C     *INKJ         IFEQ      *ON
     C                   MOVEA     '00'          *IN(41)
     C                   SETON                                        90
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C                   EXSR      SRCHGP
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   EXSR      SRCHEM
      **** BEGIN ADD **** 11/17/99 ************************************
     C     HFLD          WHENEQ    'SCFEIN'
     C                   EXSR      SRCHFI
      **** BEGIN ADD **** 11/17/99 ************************************
     C*** BEGIN ADD *** 11/13/02 **************************************
     C     HFLD          WHENEQ    'SCOFF#'
     C                   EXSR      SROFF#
     C*** END   ADD *** 11/13/02 **************************************
     C                   OTHER
     C                   SETON                                        75
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      ***** BEG CHG ***** 07/29/04 ************************************
     C****************     EXSR NXTPGM                     ADD 04/01/03
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      NXTPGM                                       ADD 04/01/03
     C                   ENDIF                                                  END *INKC/*INKL
      *
      * If *IN90 passed back from F0004R02, Then reset F12 Indicator
     C     *IN90         IFEQ      *ON
     C                   MOVE      *ON           *INKL
     C                   ENDIF
      ***** END CHG ***** 07/29/04 ************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
      ***** BEG ADD ***** 07/29/04 ************************************
     C     *INKC         IFEQ      *ON
     C                   EXSR      PGMEND
     C                   ENDIF                                                  END *INKC
      ***** END ADD ***** 07/29/04 ************************************
     C*
     C*** BEG ADD *** 05/28/99 *************************
     C*
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
      *** BEG ADD *** 11/12/02 *************************************************
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C*
     C                   SETOFF                                       43
     C     ##MGRI        IFEQ      'Y'
     C                   SETON                                        43
     C                   ENDIF
     C*
      *** END ADD *** 11/12/02 *************************************************
     C*** END ADD *** 05/28/99 *************************
     C*
     C                   MOVEA     '00'          *IN(41)
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* SRCHEM - Employer/division search
     C*================================================================
     C     SRCHEM        BEGSR
     C*
     C***  BEGIN ADD  ***  03/18/99  *********************************
     C                   CLEAR                   DBRRN
     C***  END   ADD  ***  03/18/99  *********************************
     C*
     C     FNKEY         CHAIN     PFN01                              99
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C***  BEGIN CHG  ***  03/18/99  *********************************
     C**********           PARM DBRRN     ##RRN   90
     C**********           PARM           ##RRN2  90
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C***  END   CHG  ***  03/18/99  *********************************
     C                   PARM                    ##QTE             1
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMEMP                              99
     C*
     C  N99              MOVEL     EMEMP#        SCEMP#
     C  N99              MOVEL     EMDIV         SCDIV
     C*
     C***  BEGIN ADD  ***  03/18/99  *********************************
     C     SCFND         IFEQ      *ZEROS
     C     *IN99         ANDEQ     *OFF
     C                   Z-ADD     EMFND         SCFND
     C                   ENDIF                                                  SCFND=*ZEROS
     C***  END   ADD  ***  03/18/99  *********************************
     C*
     C                   SETON                                        42
     C                   ENDIF
     C                   ENDSR
      **** BEGIN ADD **** 11/17/99 ************************************
     C*================================================================
     C* SRCHFI - Federal ID number search
     C*================================================================
     C     SRCHFI        BEGSR
     C*
     C                   CLEAR                   DBRRN
     C*
     C     SCFND         IFNE      *ZEROS
     C     FNKEY         CHAIN     PFN01                              99
     C                   Z-ADD     DBRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     *ZEROS        ##RRN
     C                   ENDIF
     C*
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C     DBRRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WMAAP                              99
     C  N99              Z-ADD     AAFEIN        SCFEIN
     C*
     C                   ELSE
     C     ##RRN         CHAIN     WMAGP                              99
     C  N99              Z-ADD     AGFEIN        SCFEIN
     C                   ENDIF
     C*
     C                   ENDSR
      **** END   ADD **** 11/17/99 ************************************
     C*================================================================
     C* SRCHGP - Group number search
     C*================================================================
     C     SRCHGP        BEGSR
     C                   MOVEL     UPCO#         ##CO#A
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#A            3
      **** BEGIN CHG **** 08/24/00 *********************************************
     C******************** PARM           ##FNDA  3
     C                   PARM      *BLANKS       ##FNDA            3
      **** END   CHG **** 08/24/00 *********************************************
     C                   PARM                    ##FYR             3
      **** BEGIN CHG **** 08/24/00 *********************************************
     C********** ##FND     IFNE *ZEROS
     C     ##FNDA        IFNE      *ZEROS
     C     ##FNDA        ANDNE     *BLANKS
      **** END   CHG **** 08/24/00 *********************************************
     C                   MOVEL     ##FNDA        SCFND
     C                   SETON                                        41
      **** BEGIN ADD **** 08/24/00 *********************************************
     C                   ELSE
     C                   SETON                                        40
      **** END   ADD **** 08/24/00 *********************************************
     C                   ENDIF
     C                   ENDSR
     C*** BEGIN ADD *** 11/13/02 **************************************
     C*================================================================
     C* SROFF# - Office Location search
     C*================================================================
     C     SROFF#        BEGSR
     C*
     C                   CALL      'W0002R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##OFF#            5
     C*
     C     *INKL         IFEQ      *ON
     C     ##OFF#        OREQ      *BLANKS
     C                   SETON                                        4590
     C                   ELSE
     C                   SETON                                          90
     C                   MOVEL     ##OFF#        SCOFF#
     C                   MOVEL     *BLANKS       SCOFFN
     C     OFKY          CHAIN     POF01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCOFFN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 11/13/02 **************************************
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     UPUSR         CHAIN     PUP01                              98
B01  C     *IN98         IFEQ      *OFF
E01  C                   ENDIF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     Q1USER        UPUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C                   MOVEL     DSTIME        UPTIME
     C                   UPDATE    DBRCDF
     C                   UPDATE    PUP01
     C*** BEG DEL *** 04/01/03 ****************************
     C********             EXSR UPWCMS
     C********             EXSR NXTPGM                     ADD 05/28/99
     C*** END DEL *** 04/01/03 ****************************
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPWCMS - Update WCMS user parmeters
     C*================================================================
     C     UPWCMS        BEGSR
     C*
     C                   CALL      'WA025R'                             99
     C                   PARM                    UPUSR
     C                   PARM      *BLANKS       ##DLT             1
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      'A'           UPSTAT
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     Q1USER        UPIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C                   WRITE     DBRCDF
     C                   WRITE     PUP01
     C                   FEOD      SMUPP
     C                   EXSR      UPWCMS
     C*** BEG DEL *** 04/01/03 ****************************
     C************         EXSR NXTPGM                     ADD 05/28/99
     C*** END DEL *** 04/01/03 ****************************
     C                   ENDSR
