     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H******************************************************************
     H* REVISIONS:                                                     *
     H*
     H*                                                                *
     H******************************************************************
     FW7416F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMSHP     UF   E             DISK
     F                                     RENAME(PSH01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCPOLS State Submi-
     D                                     ssion Status')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@MODE            1
     C                   PARM      *INKC         @@KC              1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C*
     C* FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C*
     C                   ENDSR                                                  CHGSTS
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
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C  N90SCST01        IFNE      *BLANKS
     C     *IN11         ANDEQ     *OFF
     C     SCSS01        IFNE      '3'
     C     SCSS01        ANDNE     '4'
     C                   SETON                                        42  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST02        IFNE      *BLANKS
     C     *IN12         ANDEQ     *OFF
     C     SCSS02        IFNE      '3'
     C     SCSS02        ANDNE     '4'
     C                   SETON                                        43  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST03        IFNE      *BLANKS
     C     *IN13         ANDEQ     *OFF
     C     SCSS03        IFNE      '3'
     C     SCSS03        ANDNE     '4'
     C                   SETON                                        44  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST04        IFNE      *BLANKS
     C     *IN14         ANDEQ     *OFF
     C     SCSS04        IFNE      '3'
     C     SCSS04        ANDNE     '4'
     C                   SETON                                        45  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST05        IFNE      *BLANKS
     C     *IN15         ANDEQ     *OFF
     C     SCSS05        IFNE      '3'
     C     SCSS05        ANDNE     '4'
     C                   SETON                                        46  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST06        IFNE      *BLANKS
     C     *IN16         ANDEQ     *OFF
     C     SCSS06        IFNE      '3'
     C     SCSS06        ANDNE     '4'
     C                   SETON                                        47  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST07        IFNE      *BLANKS
     C     *IN17         ANDEQ     *OFF
     C     SCSS07        IFNE      '3'
     C     SCSS07        ANDNE     '4'
     C                   SETON                                        48  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST08        IFNE      *BLANKS
     C     *IN18         ANDEQ     *OFF
     C     SCSS08        IFNE      '3'
     C     SCSS08        ANDNE     '4'
     C                   SETON                                        49  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST09        IFNE      *BLANKS
     C     *IN19         ANDEQ     *OFF
     C     SCSS09        IFNE      '3'
     C     SCSS09        ANDNE     '4'
     C                   SETON                                        50  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST10        IFNE      *BLANKS
     C     *IN20         ANDEQ     *OFF
     C     SCSS10        IFNE      '3'
     C     SCSS10        ANDNE     '4'
     C                   SETON                                        51  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST11        IFNE      *BLANKS
     C     *IN21         ANDEQ     *OFF
     C     SCSS11        IFNE      '3'
     C     SCSS11        ANDNE     '4'
     C                   SETON                                        52  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST12        IFNE      *BLANKS
     C     *IN22         ANDEQ     *OFF
     C     SCSS12        IFNE      '3'
     C     SCSS12        ANDNE     '4'
     C                   SETON                                        53  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST13        IFNE      *BLANKS
     C     *IN23         ANDEQ     *OFF
     C     SCSS13        IFNE      '3'
     C     SCSS13        ANDNE     '4'
     C                   SETON                                        54  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST14        IFNE      *BLANKS
     C     *IN24         ANDEQ     *OFF
     C     SCSS14        IFNE      '3'
     C     SCSS14        ANDNE     '4'
     C                   SETON                                        55  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCST15        IFNE      *BLANKS
     C     *IN25         ANDEQ     *OFF
     C     SCSS15        IFNE      '3'
     C     SCSS15        ANDNE     '4'
     C                   SETON                                        56  90
     C                   ENDIF
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      SHST01        SCST01
     C                   MOVE      SHSS01        SCSS01
     C                   MOVE      SHST02        SCST02
     C                   MOVE      SHSS02        SCSS02
     C                   MOVE      SHST03        SCST03
     C                   MOVE      SHSS03        SCSS03
     C                   MOVE      SHST04        SCST04
     C                   MOVE      SHSS04        SCSS04
     C                   MOVE      SHST05        SCST05
     C                   MOVE      SHSS05        SCSS05
     C                   MOVE      SHST06        SCST06
     C                   MOVE      SHSS06        SCSS06
     C                   MOVE      SHST07        SCST07
     C                   MOVE      SHSS07        SCSS07
     C                   MOVE      SHST08        SCST08
     C                   MOVE      SHSS08        SCSS08
     C                   MOVE      SHST09        SCST09
     C                   MOVE      SHSS09        SCSS09
     C                   MOVE      SHST10        SCST10
     C                   MOVE      SHSS10        SCSS10
     C                   MOVE      SHST11        SCST11
     C                   MOVE      SHSS11        SCSS11
     C                   MOVE      SHST12        SCST12
     C                   MOVE      SHSS12        SCSS12
     C                   MOVE      SHST13        SCST13
     C                   MOVE      SHSS13        SCSS13
     C                   MOVE      SHST14        SCST14
     C                   MOVE      SHSS14        SCSS14
     C                   MOVE      SHST15        SCST15
     C                   MOVE      SHSS15        SCSS15
     C*
     C     SHST01        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   MOVE      *ON           *IN26
     C                   ELSE
     C     SHSS01        IFEQ      '4'
     C     SHSS01        OREQ      'E'
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST02        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN12
     C                   ELSE
     C     SHSS02        IFEQ      '4'
     C     SHSS02        OREQ      'E'
     C                   MOVE      *ON           *IN12
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST03        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN13
     C                   ELSE
     C     SHSS03        IFEQ      '4'
     C     SHSS03        OREQ      'E'
     C                   MOVE      *ON           *IN13
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST04        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN29
     C                   MOVE      *ON           *IN14
     C                   ELSE
     C     SHSS04        IFEQ      '4'
     C     SHSS04        OREQ      'E'
     C                   MOVE      *ON           *IN14
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST05        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN15
     C                   ELSE
     C     SHSS05        IFEQ      '4'
     C     SHSS05        OREQ      'E'
     C                   MOVE      *ON           *IN15
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST06        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN16
     C                   ELSE
     C     SHSS06        IFEQ      '4'
     C     SHSS06        OREQ      'E'
     C                   MOVE      *ON           *IN16
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST07        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN17
     C                   ELSE
     C     SHSS07        IFEQ      '4'
     C     SHSS07        OREQ      'E'
     C                   MOVE      *ON           *IN17
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST08        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN18
     C                   ELSE
     C     SHSS08        IFEQ      '4'
     C     SHSS08        OREQ      'E'
     C                   MOVE      *ON           *IN18
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST09        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN19
     C                   ELSE
     C     SHSS09        IFEQ      '4'
     C     SHSS09        OREQ      'E'
     C                   MOVE      *ON           *IN19
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST10        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN20
     C                   ELSE
     C     SHSS10        IFEQ      '4'
     C     SHSS10        OREQ      'E'
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST11        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN21
     C                   ELSE
     C     SHSS11        IFEQ      '4'
     C     SHSS11        OREQ      'E'
     C                   MOVE      *ON           *IN21
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST12        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN37
     C                   MOVE      *ON           *IN22
     C                   ELSE
     C     SHSS12        IFEQ      '4'
     C     SHSS12        OREQ      'E'
     C                   MOVE      *ON           *IN22
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST13        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN23
     C                   ELSE
     C     SHSS13        IFEQ      '4'
     C     SHSS13        OREQ      'E'
     C                   MOVE      *ON           *IN23
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST14        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN39
     C                   MOVE      *ON           *IN24
     C                   ELSE
     C     SHSS14        IFEQ      '4'
     C     SHSS14        OREQ      'E'
     C                   MOVE      *ON           *IN24
     C                   ENDIF
     C                   ENDIF
     C*
     C     SHST15        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN25
     C                   ELSE
     C     SHSS15        IFEQ      '4'
     C     SHSS15        OREQ      'E'
     C                   MOVE      *ON           *IN25
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      SCST01        SHST01
     C                   MOVE      SCSS01        SHSS01
     C                   MOVE      SCST02        SHST02
     C                   MOVE      SCSS02        SHSS02
     C                   MOVE      SCST03        SHST03
     C                   MOVE      SCSS03        SHSS03
     C                   MOVE      SCST04        SHST04
     C                   MOVE      SCSS04        SHSS04
     C                   MOVE      SCST05        SHST05
     C                   MOVE      SCSS05        SHSS05
     C                   MOVE      SCST06        SHST06
     C                   MOVE      SCSS06        SHSS06
     C                   MOVE      SCST07        SHST07
     C                   MOVE      SCSS07        SHSS07
     C                   MOVE      SCST08        SHST08
     C                   MOVE      SCSS08        SHSS08
     C                   MOVE      SCST09        SHST09
     C                   MOVE      SCSS09        SHSS09
     C                   MOVE      SCST10        SHST10
     C                   MOVE      SCSS10        SHSS10
     C                   MOVE      SCST11        SHST11
     C                   MOVE      SCSS11        SHSS11
     C                   MOVE      SCST12        SHST12
     C                   MOVE      SCSS12        SHSS12
     C                   MOVE      SCST13        SHST13
     C                   MOVE      SCSS13        SHSS13
     C                   MOVE      SCST14        SHST14
     C                   MOVE      SCSS14        SHSS14
     C                   MOVE      SCST15        SHST15
     C                   MOVE      SCSS15        SHSS15
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
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C                   Z-ADD     SHCO#         ##CO#
     C                   Z-ADD     SHFND         ##FND
     C                   Z-ADD     SHFND         SCFND
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      SHEMP#        XXEMP#
     C                   PARM      SHDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   MOVEA     '00000'       *IN(11)
     C                   MOVEA     '00000'       *IN(16)
     C                   MOVEA     '00000'       *IN(21)
     C                   MOVEA     '00000'       *IN(26)
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C                   MOVEA     '0'           *IN(41)
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
