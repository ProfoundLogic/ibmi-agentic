     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  F0004R - WCMC/FSMS USER PARAMETERS  C:62 02/23/98            *
     H*                                                               *
     H*  CALLED FROM: MENU                                            *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/06/15 C:267 Support: 37326                                 *
     H*                Division restriction should be dependent on    *
     H*                employer restriction.                          *
     H*                                                               *
     H* 10/28/02 C:62 PROJECT MC253                                   *
     H*               ADD MULTI GROUP RESTRICTIONS LOGIC.             *
     H*                                                               *
     H* 05/16/00 C:33 SEQ#: OP082/21740                               *
     H*               -CHANGE SCDIV FROM NUMERIC TO ALPHA (NUMBERS    *
     H*                ONLY) TO ALLOW ALL DIVISIONS OR ONE DIVISION TO*
     H*                BE RESTRICTED.                                 *
     H*               -DIVISION RESTRICTION WILL BE LOADED TO UPFL5   *
     H*                (BEHIND THE SCENES).                           *
     H*                                                               *
     H* 02/04/00 C:61  PROJECT MC205                                  *
     H*                Pass SCSUB# to Group Search.                   *
     H*                                                               *
     H*  06/09/99  C:53  MC145
     H*                  CONDITION CALL TO WA025R TO BE BASED ON      *
     H*                  PDS WORKERS COMP BEING INSTALLED.            *
     H*                                                               *
     H*  10/07/98 C:94 PROJECT: 87MFX                                 *
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL              *
     H*                                                               *
     H*****************************************************************
     FF0004F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFMSML     IF   E           K DISK
      ***** BEG ADD ******* 02/04/00 **********************************
     F                                     INFDS(SMFDS)
      ***** END ADD ******* 02/04/00 **********************************
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMEML     IF   E           K DISK
     F*  Physical file definition (for maintaining record)
     FSMUPP     UF A E             DISK
     F                                     RENAME(PUP01:DBRCDF)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      /COPY SCOPYMT01D
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
      ***** BEG ADD ******* 02/04/00 **********************************
     D SMFDS           DS
     D  SMRRN                397    400B 0
      ***** END ADD ******* 02/04/00 **********************************
     D*
     D***  BEGIN ADD  ***  05/16/00  *********************************
     D CHGNUM          C                   CONST('SA020R')
     D*
     D                 DS
     D  SCDIV                  1      5
     D  XXDIV1                 1      1
     D  XXDIV2                 2      2
     D  XXDIV3                 3      3
     D  XXDIV4                 4      4
     D  XXDIV5                 5      5
     D***  END   ADD  ***  05/16/00  *********************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC/FSMS User Para-
     D                                     meters')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

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
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     SMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     EMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C***  END   CHG  ***  05/16/00  *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UPUSR         ##USR
     C     *LIKE         DEFINE    UPUSNM        ##USNM
     C     *LIKE         DEFINE    UPOUTQ        ##OUTQ
     C     *LIKE         DEFINE    UPSUB#        KKSUB#
     C     *LIKE         DEFINE    UPFND         KKFND
     C     *LIKE         DEFINE    UPEMP#        KKEMP#
     C     *LIKE         DEFINE    UPDIV         KKDIV
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     *LIKE         DEFINE    SCDIV         SVDIV
     C     *LIKE         DEFINE    SCEMP#        SVEMP#
     C     *LIKE         DEFINE    SCMREM        SVMREM
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEGIN ADD *** 10/28/02 **************************************
     C*================================================================
     C* CHKAZP - Check SMAZP
     C*================================================================
     C     CHKAZP        BEGSR
     C*
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      SCUSR         ##USR
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C                   SETOFF                                       17
     C     ##RCDS        IFEQ      *ON
     C                   SETON                                        17
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 10/28/02 **************************************
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* RESET PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0'           *IN(36)
     C*
     C* SUBSCRIBER RESTRICTION FLAG MUST BE 'Y' OR 'N'.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCMRSU        IFNE      'Y'
     C     SCMRSU        ANDNE     'N'
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* SUBSCRIBER NUMBER MUST BE VALID AND ACTIVE
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCMRSU        IFEQ      'Y'
     C     SCFND         ORNE      *ZEROS
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7290
     C                   ELSE
     C     SMSTAT        IFEQ      'I'
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* GROUP NUMBER RESTRICTION FLAG MUST BE 'Y' OR 'N'.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCMRGR        IFNE      'Y'
     C     SCMRGR        ANDNE     'N'
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* GROUP NUMBER MUST BE VALID AND ACTIVE
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCMRGR        IFEQ      'Y'
     C     SCFND         ORNE      *ZEROS
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7490
     C                   ELSE
     C     FNSTAT        IFEQ      'I'
     C                   SETON                                        7490
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* EMPLOYER/DIVISION RESTRICTION FLAG MUST BE 'Y' OR 'N'.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCMREM        IFNE      'Y'
     C     SCMREM        ANDNE     'N'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* EMPLOYER/DIVISION NUMBER MUST BE VALID AND ACTIVE
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCMREM        IFEQ      'Y'
     C     SCEMP#        ORNE      *ZEROS
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
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
     C                   MOVE      SCDIV         KKDIV
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   ELSE
     C*
     C     EMKY2         CHAIN     PEM01                              99
     C*
     C                   ENDIF                                                  SCDIV<>*BLANKS
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        7690
     C                   ELSE
     C     EMSTAT        IFEQ      'I'
     C                   SETON                                        7690
     C                   ENDIF
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C  N90SCEMP#        IFEQ      *ZEROS
     C     SCDIV         ANDNE     *BLANKS
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCEMP#=*ZEROS
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   ENDIF
     C                   ENDIF
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C*
     C*  CHECK FOR DIV=*BLANK FIRST TIME THRU AND SEND MESSAGE
     C*
     C  N90SCEMP#        IFNE      *ZEROS
     C     SCDIV         ANDEQ     *BLANKS
     C     SCMREM        ANDEQ     'Y'
     C*
     C     SCDIV         IFNE      SVDIV
     C     SCEMP#        ORNE      SVEMP#
     C     SCMREM        ORNE      SVMREM
     C                   SETON                                        89  90
     C                   MOVE      SCDIV         SVDIV
     C                   MOVE      SCEMP#        SVEMP#
     C                   MOVE      SCMREM        SVMREM
     C                   ENDIF                                                  SCDIV<>SVDIV
     C*
     C                   ENDIF                                                  SCEMP#<>*ZEROS
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C*** BEGIN ADD *** 10/28/02 **************************************
     C  N90SCMRSU        IFEQ      'N'
     C     SCMRGR        ANDEQ     'N'
     C     SCMREM        ANDEQ     'N'
     C*
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      SCUSR         #@USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C     ##RCDS        IFEQ      *ON
     C                   SETON                                        88  90
     C                   ENDIF                                                  ##RCDS=*ON
     C                   ENDIF                                                  SCMRSU=N
     C*** END   ADD *** 10/28/02 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
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
     C*** BEGIN ADD *** 10/28/02 **************************************
     C*================================================================
     C* KHDO   - Processing for F8
     C*================================================================
     C*
     C*  F8=RESTRICTIONS
     C*
     C     KHDO          BEGSR
     C*
     C     SCMRSU        IFNE      'Y'
     C     SCMRGR        ANDNE     'Y'
     C     SCMREM        ANDNE     'Y'
     C*
     C                   SETON                                        88  90
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      *ON
     C                   MOVE      '1'           ##CALL
     C                   ELSE
     C                   MOVE      ' '           ##CALL
     C                   ENDIF                                                  *IN09=*ON
     C*
     C                   CALL      'S0009R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      'M'           ##APPL            1
     C                   PARM      SCUSR         ##USR
     C*
     C                   EXSR      CHKAZP
     C*
     C                   ENDIF                                                  SCXFND<>Y<>SCXEMP
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   ENDIF                                                  *INKC=*ON
     C*
     C                   ENDSR                                                  KHDO
     C*** END   ADD *** 10/28/02 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* RESET PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0'           *IN(36)
     C*
     C                   SELECT
     C*
     C* SUBSCRIBER SEARCH
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C*** BEGIN CHG *** 03/05/99 **************************************
     C**********           PARM '2'       ##CALL  1
     C                   PARM      '*'           ##CALL            1
     C*** END   CHG *** 03/05/99 **************************************
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C*
     C                   SETON                                        31
     C                   MOVE      SMSUB#        SCSUB#
     C*
     C                   ENDIF
     C*
     C* GROUP SEARCH
     C*
     C     HFLD          WHENEQ    'SCFND'
      ***** BEG ADD ******* 02/04/00 **********************************
      *
     C                   MOVE      SCSUB#        KKSUB#
     C     SMKY          CHAIN     PSM01                              72
     C   72              Z-ADD     *ZERO         SMRRN
      *
     C     *IN72         IFEQ      *OFF
      ***** END ADD ******* 02/04/00 **********************************
     C*
     C                   CALL      'F0060R'
     C*** BEGIN CHG *** 03/05/99 **************************************
     C**********           PARM '2'       ##CALL
     C                   PARM      '*'           ##CALL
     C*** END   CHG *** 03/05/99 **************************************
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
      ***** BEG CHG ******* 02/04/00 **********************************
     C****************     PARM *ZEROS    ##RRN
     C                   PARM      SMRRN         ##RRN
      ***** END CHG ******* 02/04/00 **********************************
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PFN02                              99
     C*
     C                   SETON                                        33
     C                   MOVE      FNFND         SCFND
     C*
     C                   ENDIF
      ***** BEG ADD ******* 02/04/00 **********************************
      *
     C                   ENDIF                                                  *IN72 = *OFF
      ***** END ADD ******* 02/04/00 **********************************
     C*
     C* EMPLOYER/DIVISION SEARCH
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F0140R'
     C***  BEGIN CHG  ***  03/05/99  *********************************
     C***********          PARM '2'       ##CALL
     C                   PARM      '*'           ##CALL
     C***  END   CHG  ***  03/05/99  *********************************
     C     *INKC         PARM      '0'           ##KC
     C     *INKC         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN                          FMEMP
     C                   PARM      DBRRN         ##RRN2            9 0          FMFNP
     C                   PARM      3             ##VIEW            1 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PEM02                              99
     C*
     C                   SETON                                        35
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   ENDIF                                                  *INKL EQ '1'
     C*
     C                   ELSE
     C*
     C                   SETON                                        77
     C*
     C                   ENDIF                                                  *IN99 EQ '0'
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   MOVEL     UPFYR         SCFYR
     C                   MOVEL     UPSUB#        SCSUB#
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     UPEMP#        SCEMP#
     C***  BEGIN DEL  ***  05/16/00  *********************************
     C**********           MOVELUPDIV     SCDIV
     C***  END   DEL  ***  05/16/00  *********************************
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   Z-ADD     SCEMP#        SVEMP#
     C*
     C*** BEGIN ADD *** 10/28/02 **************************************
     C                   SETOFF                                       17
     C     *IN16         IFEQ      *ON
     C                   EXSR      CHKAZP
     C                   ENDIF
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C     UPFL5         IFEQ      'Y'
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
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   MOVEL     UPMRSU        SCMRSU
     C                   MOVEL     UPMRGR        SCMRGR
     C                   MOVEL     UPMREM        SCMREM
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   MOVEL     SCMREM        SVMREM
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCFYR         UPFYR
     C                   MOVEL     SCSUB#        UPSUB#
     C                   MOVEL     SCFND         UPFND
     C                   MOVEL     SCEMP#        UPEMP#
     C                   MOVEL     SCDIV         UPDIV
     C                   MOVEL     SCMRSU        UPMRSU
     C                   MOVEL     SCMRGR        UPMRGR
     C                   MOVEL     SCMREM        UPMREM
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     SCDIV         IFNE      *BLANK
     C***  BEGIN ADD  ***  11/06/15  *********************************
     C     SCMREM        ANDEQ     'Y'
     C***  END   ADD  ***  11/06/15  *********************************
     C                   MOVE      'Y'           UPFL5
     C                   ELSE
     C                   CLEAR                   UPFL5
     C                   ENDIF                                                  SCDIV<>*BLANK
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   ENDSR
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
     C*** BEGIN ADD *** 03/05/99 **************************************
     C     *INKH         CASEQ     *ON           KHDO                     90
     C*** END   ADD *** 03/05/99 **************************************
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
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
     C*** BEG ADD *** 06/09/99 *************
     C*
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
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
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** BEGIN ADD *** 10/28/02 **************************************
     C                   PARM                    ##OS              1            OUTOME/SAVINGS
     C                   PARM                    ##ARLS            1            Auto Release Flag
     C                   PARM                    ##CCOD            1            Correct Coding Flag
     C                   PARM                    ##CRVW            1            Code Review Flag
     C                   PARM                    ##PROM            1            ProMatch Flag
     C                   PARM                    ##MCRV            1            Medicare CR Flag
     C                   PARM                    ##FREQ            1            Frequency Flag
     C                   PARM                    ##CRAC            1            CR Auto Complete
     C                   PARM                    ##PREO            1            CR Auto Complete
     C                   PARM                    ##PARN            1            Par/Non-Par/Both
     C                   PARM                    ##MGRI            1            Multi Group Restr
     C*
     C                   SETOFF                                       16
     C                   SETOFF                                       17
     C     ##MGRI        IFEQ      'Y'
     C                   SETON                                        16
     C                   EXSR      CHKAZP
     C                   ENDIF
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C*** END ADD *** 06/09/99 *************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     Q1USER        UPUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C                   MOVEL     DSTIME        UPTIME
     C                   UPDATE    DBRCDF
     C                   EXSR      UPWCMS
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPWCMS - Update WCMS user parmeters
     C*================================================================
     C     UPWCMS        BEGSR
     C*
     C     ##WCIF        IFEQ      'Y'                                          ADD 06/09/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/09/99
     C                   CALL      'WA025R'                             99
     C                   PARM                    UPUSR
     C                   PARM      *BLANKS       ##DLT             1
     C                   ENDIF                                                  ADD 06/09/99
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
     C                   FEOD      SMUPP
     C                   EXSR      UPWCMS
     C                   ENDSR
