     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* Revisions:                                                    *
     H*
     H* 05/30/18 C:053 Project : WC2515
     H*                Additional contact name change
     H*                                                               *
     H* 12/27/11 C:282 Project: WC2005                                *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H*  10/20/11 C:278  PROJECT: 133WLW PRISS#28                     *
     H*                  The Dart Remark sometimes retain the         *
     H*                  previous claimant address if more than one   *
     H*                  person is changed in a row.                  *
     H*                                                               *
     H* 10/03/11 C:278 Project: 133WLW                                *
     H*                  The Dart Remark will be updated to           *
     H*                  include the claimant's old address, along    *
     H*                  with the reason for the change.              *
     H*
     H* 10/27/09 C:235 SUPPORT: 69771                                 *
     H*                releax the edit on Dependent Relationship field*
     H*                also Numerical Birth order need to show as 0 if*
     H*                the field is blank                             *
     H*                                                               *
     H* 07/14/09 C:189 Project: 114WOJ                                *
     H*                  Change of Address Tracking/Force new Darts   *
     H*                  Remark                                       *
     H*
     H* 05/16/07 C:183 PROJECT: 114WOC                                *
     H*                Address History records to be added to WDAHP.  *
     H*                In case of a 'WRITE' to WMCDP a new record is  *
     H*                added to WDAHP and in case of an 'UPDATE' to   *
     H*                WMCDP 2 records are added, both the before and *
     H*                after image.                                   *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*
     H* 09/08/02 C:142 WC1077
     H*                ADDED FIELD SCDPCY.
     H*
     H* 04/27/00 C:53  SUPPORT: WC065/19768
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 08/04/99 C:58 Project: WC855                                  *
     H*               Change name and/or address in Payment Request
     H*               file (WARPLC) also.
     H*                                                               *
     H* 02/10/99 C:113 SUPPORT: WC025 #16528                          *
     H*                Added code to correct the display of dates     *
     H*                after coming back from an F10 search.          *
     H*                                                               *
     H* 04/22/98 C:15 Project: seq# 10899                             *
     H*               Changed pgm to protect the name and address if  *
     H*               the user is not allowed to change them (UPFL2 = *
     H*               'N').                                           *
     H*****************************************************************
     FW0573F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWDACP     UF A E             DISK
     F                                     RENAME(PAC01:DBRCDF)
     F*** BEGIN ADD ***  12/27/11  ************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *****  12/27/11  ************************************
     FWDACL     IF   E           K DISK
     FWMCMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** BEGIN ADD *** 04/22/98 ***********************************************
     FWMUPL     IF   E           K DISK
     F*** END ADD ***** 04/22/98 ***********************************************
     F*** BEGIN ADD *** 08/04/99 ***********************************************
     F*
     F*     Payment request file.
     FWARPLC    UF   E           K DISK
     F*** END ADD ***** 08/04/99 ***********************************************
     F** BEG ADD *** 05/16/07 *****************************************
     FWDAHP     O    E             DISK
     F** END ADD *** 05/16/07 *****************************************
      ***** BEG ADD ******* 07/14/09 **********************************
     FWMCXL     IF   E           K DISK
      ***** END ADD ******* 07/14/09 **********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  LN1                    1     79
     D  LN1A                   1      6
     D  LN1B                   8     10
     D  LN1C                  12     71
     D*
     D                 DS
     D  LN2                    1     79
     D  SCYRCS                 8     18
     D  SCSS#                 26     36
     D  SCCLNM                39     58
     D  SCACDT                69     78
     D                 DS
     D  SCDSS#                 1      9
     D  SCSS1                  1      3
     D  SCSS2                  4      5
     D  SCSS3                  6      9
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Additional Address-
     D                                      Master Information')
     D COMAPI          C                   CONST('WA001R')
     D EMPAPI          C                   CONST('WA005R')
     D CLMAPI          C                   CONST('WA006R')
     D EXRAPI          C                   CONST('WA010R')
     D #OFSCR          C                   CONST(1)
     D XCEXRE          C                   CONST('EXCS REIN ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Claim line API
     C*
     C     CLMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##YR
     C                   PARM                    ##CASE
     C                   PARM                    LN1
     C                   PARM                    LN2
     C*
     C*  Employer Emp#/Div/Name API
     C*
     C     EMPPL         PLIST
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ENAM
     C                   PARM                    ##EDN            80
     C*
     C*  Message API
     C*
     C     EXRPL         PLIST
     C                   PARM      @@RRN         ##RRN             9 0          FOR WMCMP
     C                   PARM                    ##FNDR           13 2
     C                   PARM                    ##EXRE            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ACKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##YR
     C                   KFLD                    ##CASE
     C                   KFLD                    KKSEQ#
     C*
     C     ACKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##YR
     C                   KFLD                    ##CASE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*** BEGIN ADD *** 08/04/99 ***********************************************
     C*
     C     RPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##YR
     C                   KFLD                    ##CASE
     C*
     C*** END ADD ***** 08/04/99 ***********************************************
      ***** BEG ADD ******* 07/14/09 **********************************
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      ***** END ADD ******* 07/14/09 **********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ACSEQ#        KKSEQ#
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##YR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    EMEMP#        ##EMP#
     C     *LIKE         DEFINE    EMDIV         ##DIV
     C     *LIKE         DEFINE    EMNAME        ##ENAM
     C     *LIKE         DEFINE    GTKEY         ##KEY
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    SCPRDS        ##PRDS
     C     *LIKE         DEFINE    @@MODE        ##CALL
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     ACSTAT        IFEQ      'I'
     C                   MOVE      'A'           ACSTAT
     C                   ELSE
     C                   MOVE      'I'           ACSTAT
     C                   ENDIF                                                  ACSTAT
     C*
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C*
     C                   MOVEL     DSTIME        ACTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
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
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*   NAME CANNOT BE BLANK
     C*
     C  N90SCNAMF        IFEQ      *BLANK
     C                   SETON                                        8090
     C                   ENDIF
     C*
     C*** BEG ADD *** 05/30/18 ****************************
     C  N90SCPNAM        IFEQ      *BLANK
     C                   SETON                                        7190
     C                   ENDIF
     C*** END ADD *** 05/30/18 ****************************
     C*
     C*   ADDRESS LINE CANNOT BE BLANK
     C*
     C  N90SCADR1        IFEQ      *BLANK
     C                   SETON                                        7290
     C                   ENDIF
     C*** BEG ADD *** 10/20/03 *****************************
      *
      *  EITHER THE C,S,Z IS ENTERED OR THE OUT OF COUNTRY ADDRESS
      *
     C  N90SCCTY         IFEQ      *BLANKS
     C     SCST          ANDEQ     *BLANKS
     C     SCZIP1        ANDEQ     *ZERO
     C     SCCTRY        ANDEQ     *BLANK
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        9029
     C                   ENDIF
      *
     C     SCCTRY        IFNE      *BLANK
     C     SCCTRY        ANDNE     'USA'
      *
     C  N90SCADR3        IFEQ      *BLANK
     C                   SETON                                        9029
     C                   ENDIF
     C                   ENDIF
      *
      * BOTH THE C,S,Z AND THE OUT OF COUNTRY ADDRESS CANNOT BE ENTERED
      ***
     C***        SCCCTY    IFNE *BLANKS
     C***        SCCST     ANDNE*BLANKS
     C***        SCCZP1    ANDNE*ZERO
     C***        SCCTRY    ANDNE*BLANK
     C***        SCADR3    ANDNE*BLANK
      ***
     C***        SCCCTY    ORNE *BLANKS
     C***        SCCST     ANDNE*BLANKS
     C***        SCCZP1    ANDNE*ZERO
     C***        SCCTRY    ANDNE'USA'
     C***        SCADR3    ANDNE*BLANK
     C***                  SETON                     9030
     C***                  LEAVE
     C***                  ENDIF
      *
     C  N90SCCTRY        IFNE      'USA'
     C     SCCTRY        ANDNE     *BLANK
      *
     C  N90SCCTY         IFNE      *BLANKS
     C     SCST          ORNE      *BLANKS
     C     SCZIP1        ORNE      *ZERO
     C                   SETON                                        9029
     C                   ENDIF
      *
     C  N90SCADR3        IFEQ      *BLANK
     C                   SETON                                        9029
     C                   ENDIF
      *
     C                   ELSE
      *
     C  N90SCADR3        IFNE      *BLANKS
     C                   SETON                                        9029
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCCTRY        IFEQ      *BLANKS
     C     SCCTRY        OREQ      'USA'
     C*** END ADD *** 10/20/03 *****************************
     C*
     C*     CITY  CANNOT BE BLANK
     C*
     C  N90SCCTY         IFEQ      *BLANK
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C*   STATE CANNOT BE BLANK
     C*
     C  N90SCST          IFEQ      *BLANK
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C*  IF IS STATE IS VALID
     C*
     C  N90              MOVEL(P)  SCST          KKKEY
     C*
     C  N90STKY          CHAIN     PST01                              99
     C*
     C  N90*IN99         IFEQ      *ON
     C                   SETON                                        7590
     C                   ENDIF
     C*
     C*   ZIP CODE CANNOT BE BLANK
     C*
     C  N90SCZIP1        IFEQ      *ZERO
     C                   SETON                                        7690
     C                   ENDIF
     C*** BEG ADD *** 10/20/03 *****************************
     C                   ENDIF
     C*
     C  N90SCCTRY        IFNE      *BLANK
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  SCCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9057
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCADR3        IFEQ      *BLANK
     C     SCCTRY        IFNE      *BLANK
     C     SCCTRY        ANDNE     'USA'
     C                   SETON                                        9058
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 10/20/03 *****************************
     C*
     C*  THE DEPENDENT RELATIONSHIP FLAG MUST BE A VALID
     C*  TABLE FILE VALUE.
     C*
     C                   MOVEL     'DPRL'        KKTBID
     C                   MOVEL(P)  SCREL         KKKEY
     C*
     C*** BEG ADD *** 10/27/09 *****************************
     C  N90SCREL         IFNE      *BLANK
     C*** END ADD *** 10/27/09 *****************************
     C  N90GTKY          CHAIN     PGT01                              99
     C*
     C  N90*IN99         IFEQ      '1'
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF
     C*** BEG ADD *** 10/27/09 *****************************
     C                   ENDIF
     C*** END ADD *** 10/27/09 *****************************
     C*
     C*  DEPENDENT DATE OF BIRTH MUST BE A VALID DATE, IF ENTERED.
     C*
     C  N90SCDDOB        IFNE      *ZERO
     C*
     C                   Z-ADD     SCDDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDDOB
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7890
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  ALLOW CHECK MUST BE Y OR N.
     C*
     C  N90SCCKAL        IFNE      'Y'
     C     SCCKAL        ANDNE     'N'
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90ACDT          IFGT      SAVEDT
     C     ACDT          OREQ      SAVEDT
     C     ACTIME        ANDGT     SAVETM
     C*
     C                   SETON                                        8190
     C*
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
      ***** BEG ADD ******* 07/14/09 **********************************
     C     CXKY          CHAIN     PCX01                              99
      *
     C     CXAADP        IFNE      *BLANKS
     C                   EXSR      AUDIT
      *
     C     @@AFL         IFEQ      'Y'
     C     ##RTN         ANDNE     'Y'
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
      *
      ***** END ADD ******* 07/14/09 **********************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
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
     C*
     C*================================================================
     C* KJDO - F10=Searches
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*
     C* DEPENDENT RELATIONSHIP TABLE SEARCH
     C*
     C     HFLD          IFEQ      'SCREL'
     C*
     C                   MOVEL     'DPRL'        ##TBID
     C*
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      *BLANKS       ##CALL
     C                   PARM                    ##TBID
     C                   PARM      *BLANKS       ##KEY
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ENDIF
     C*
     C     ##RTCD        COMP      'B'                                    82
     C     ##KEY         COMP      *BLANKS                                82
     C*
     C     ##RTCD        IFEQ      '*'
     C*
     C                   MOVEL     ##KEY         SCREL
     C                   MOVE      *BLANK        SCRELD
     C*
     C                   MOVEL     'DPRL'        KKTBID
     C                   MOVEL(P)  SCREL         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTDESC        SCRELD
     C                   MOVE      '1'           *IN83
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*** BEGIN ADD *** 02/10/99 **************************************
     C*
     C*  Run the date through DATCHK for the editing on the display
     C*
     C     SCDDOB        IFNE      *ZERO
     C*
     C                   Z-ADD     SCDDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDDOB
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7890
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 02/10/99 **************************************
     C*** BEG ADD *** 10/20/03 ****************************
     C*
     C     HFLD          IFEQ      'SCCTRY'
     C                   MOVEL(P)  'CTRY'        ##TBID
     C                   MOVE(P)   *BLANK        ##KEY
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM                    ##CALL
     C                   PARM                    ##TBID
     C                   PARM                    ##KEY
     C                   PARM                    ##RTCD
     C     ##RTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     ##RTCD        IFEQ      'B'
     C     ##KEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN38                          POS CUSR
     C                   END
     C     ##RTCD        IFEQ      '*'
     C                   MOVEL(P)  ##KEY         SCCTRY
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  ##KEY         KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   MOVE      *ON           *IN39                          POS CUSR
     C                   END
     C                   END
     C                   ENDIF
     C*
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C*** BEG ADD *** 05/30/18 ****************************
     C                   MOVEL     ACFNAM        SCNAMF
     C*** END ADD *** 05/30/18 ****************************
     C                   MOVEL     ACPNAM        SCPNAM
     C                   MOVEL     ACADR1        SCADR1
     C                   MOVEL     ACADR2        SCADR2
     C                   MOVEL     ACCTY         SCCTY
     C                   MOVEL     ACST          SCST
     C                   MOVEL     ACZIP         SCZIP1
     C                   MOVE      ACZIP         SCZIP2
     C*** BEG ADD *** 10/20/03 ****************************
     C*
     C                   MOVEL(P)  ACCTRY        SCCTRY
     C                   MOVEL(P)  ACADR3        SCADR3
     C                   MOVEL(P)  ACCTRY        KKKEY
     C                   MOVEL(P)  'CTRY'        KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVE      *BLANKS       SCCTRD
     C                   ENDIF
     C*
     C*** END ADD *** 10/20/03 ****************************
     C                   MOVE      ACSEQ#        SCSEQ#
     C                   MOVEL     ACREL         SCREL
     C*
     C                   MOVEL     'DPRL'        KKTBID
     C                   MOVEL(P)  SCREL         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTDESC        SCRELD
     C*
     C                   MOVEL     ACORD         SCORD
     C                   MOVEL     ACDSS#        SCDSS#
     C*
     C                   Z-ADD     ACDDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDDOB
     C*
     C*** BEGIN ADD *** 09/08/02 **************************************
     C                   MOVEL     ACDPCY        SCDPCY
     C*** END   ADD *** 09/08/02 **************************************
     C                   MOVEL     ACCKAL        SCCKAL
     C                   MOVEL     ACSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C* ASSIGN NEXT SEQUENCE NUMBER
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     ACKY          SETGT     PAC01
     C     ACKY1         READPE    PAC01                                  99
     C*
     C     ACSEQ#        ADD       1             SCSEQ#
     C*
     C*** BEG ADD *** 05/30/18 ****************************
     C                   MOVE      *BLANKS       SCNAMF
     C*** END ADD *** 05/30/18 ****************************
     C                   MOVE      *BLANKS       SCPNAM
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *ZEROS        SCZIP1
     C                   MOVE      *ZEROS        SCZIP2
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVE      *BLANKS       SCCTRY
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCADR3
     C*** END ADD *** 10/20/03 ****************************
     C                   MOVE      *BLANKS       SCREL
     C                   MOVE      *BLANKS       SCRELD
     C                   MOVE      *BLANKS       SCORD
     C                   MOVE      *BLANKS       SCDSS#
     C                   MOVE      *ZEROS        SCDDOB
     C*** BEGIN ADD *** 09/08/02 **************************************
     C                   MOVEL     *BLANKS       SCDPCY
     C*** END   ADD *** 09/08/02 **************************************
     C                   MOVE      *BLANKS       SCCKAL
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 05/30/18 ****************************
     C                   MOVEL     SCNAMF        SVNAMF           15
     C*** END ADD *** 05/30/18 ****************************
      ***** BEG ADD ******* 07/14/09 **********************************
     C                   MOVEL     SCPNAM        SVPNAM           40
     C                   MOVEL     SCADR1        SVADR1           40
     C                   MOVEL     SCADR2        SVADR2           40
     C                   MOVEL     SCCTY         SVCTY            25
     C                   MOVEL     SCST          SVST              2
     C                   MOVEL     SCZIP1        SVZIP1            5 0
     C                   MOVE      SCZIP2        SVZIP2            4 0
     C                   MOVE      SCCTRY        SVCTRY            3
      ***** END ADD ******* 07/14/09 **********************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         ACCO#
     C                   MOVE      ##FND         ACFND
     C                   MOVE      ##YR          ACFYR
     C                   MOVE      ##CASE        ACCASE
     C*** BEG ADD *** 05/30/18 ****************************
     C                   MOVEL     SCNAMF        ACFNAM
     C*** END ADD *** 05/30/18 ****************************
     C                   MOVEL     SCPNAM        ACPNAM
     C                   MOVEL     SCADR1        ACADR1
     C                   MOVEL     SCADR2        ACADR2
     C                   MOVEL     SCCTY         ACCTY
     C                   MOVEL     SCST          ACST
     C                   MOVEL     SCZIP1        ACZIP
     C                   MOVE      SCZIP2        ACZIP
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVEL(P)  SCCTRY        ACCTRY
     C                   MOVEL(P)  SCADR3        ACADR3
     C*** END ADD *** 10/20/03 ****************************
     C                   MOVE      SCSEQ#        ACSEQ#
     C                   MOVEL     SCREL         ACREL
     C*** BEG ADD *** 10/27/09 *****************************
     C     SCORD         IFEQ      *BLANK
     C                   MOVE      *ZEROS        ACORD
     C                   ELSE
     C*** END ADD *** 10/27/09 *****************************
     C                   MOVEL     SCORD         ACORD
     C*** BEG ADD *** 10/27/09 *****************************
     C                   ENDIF
     C*** END ADD *** 10/27/09 *****************************
     C                   MOVEL     SCDSS#        ACDSS#
     C*
     C                   Z-ADD     SCDDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDDOB
     C*
     C*** BEGIN ADD *** 09/08/02 **************************************
     C                   MOVEL     SCDPCY        ACDPCY
     C*** END   ADD *** 09/08/02 **************************************
     C                   MOVEL     SCCKAL        ACCKAL
     C                   MOVEL     SCSTAT        ACSTAT
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
     C     *INKJ         CASEQ     *ON           KJDO
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
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
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
     C* CHAIN OUT TO CLAIM FILE
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C* GET EXCESS REINSURANCE INFO
     C*
     C     CMEXCS        IFNE      'Y'
     C     ##RRN         IFNE      *ZERO
     C*
     C                   CALL      EXRAPI        EXRPL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##EXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C*
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C*
     C                   ENDIF
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     CMCVT#        IFNE      *BLANK
     C*
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C*
     C                   ENDIF
     C*
     C*  AGENGY CLAIM# (OR, SATE CLAIM#)
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
     C*
     C* CALL CLAIMS API
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##YR
     C                   MOVE      CMCASE        ##CASE
     C*
     C                   CALL      CLMAPI        CLMPL
     C*
     C* CHAIN OUT TO EMPLOYER FILE TO GET EMPLOYER NAME
     C*
     C                   MOVE      CMEMP#        ##EMP#
     C                   MOVE      CMDIV         ##DIV
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   MOVEL     EMNAME        ##ENAM
     C*
     C* CALL EMPLOYER API TO COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      EMPAPI        EMPPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C* CHAIN OUT TO GROUP MASTER TO GET GROUP NAME
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C*** BEGIN ADD *** 04/22/98 ***********************************************
     C     Q1USER        CHAIN     PUP01                              99
     C     *IN99         IFEQ      '1'
     C     UPFL2         ORNE      'Y'
     C                   MOVE      '1'           *IN32
     C                   ENDIF
     C*** END ADD ***** 04/22/98 ***********************************************
     C*** Begin add *** 05/16/07 ***********************************************
     C                   TIME                    CHGDTM           14 0
     C                   MOVEL     CHGDTM        @AHTIM            6 0
     C                   MOVE      CHGDTM        @AHDT             8 0
     C                   Z-ADD     @AHDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@AHDT            7 0
     C*** End add ***** 05/16/07 ***********************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
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
     C*** BEGIN ADD *** 08/04/99 ***********************************************
     C*
     C*      Make change to Request Payments also
     C     RPKY          CHAIN     PRP01                              99
B02  C     *IN99         DOWEQ     *OFF
B03  C     ACPNAM        IFEQ      RPPNAM
     C     ACADR1        ANDEQ     RPADR1
     C     ACADR2        ANDEQ     RPADR2
     C     ACCTY         ANDEQ     RPCTY
     C     ACST          ANDEQ     RPST
     C     ACZIP         ANDEQ     RPZIP
     C*** BEG ADD *** 10/20/03 ****************************
B03  C     ACPNAM        OREQ      RPPNAM
     C     ACADR1        ANDEQ     RPADR1
     C     ACADR2        ANDEQ     RPADR2
     C     ACCTRY        ANDEQ     RPCTRY
     C     ACADR3        ANDEQ     RPADR3
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   MOVEL     SCPNAM        RPPNAM
     C*** BEG ADD *** 05/30/18 ****************************
     C     SCNAMF        IFNE      *BLANK
     C     SCNAMF        CAT(P)    SCPNAM:1      RPPNAM
     C                   ENDIF
     C*** END ADD *** 05/30/18 ****************************
     C                   MOVEL     SCADR1        RPADR1
     C                   MOVEL     SCADR2        RPADR2
     C                   MOVEL     SCCTY         RPCTY
     C                   MOVEL     SCST          RPST
     C                   MOVEL     SCZIP1        RPZIP
     C                   MOVE      SCZIP2        RPZIP
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
     C*
     C                   UPDATE    PRP01
E03  C                   ENDIF                                                  . ACNAM=RP
     C     RPKY          READE     PRP01                                  99
E02  C                   ENDDO                                                  . 99 DOW
     C*
     C*** END ADD ***** 08/04/99 ***********************************************
     C*
     C*** BEGIN ADD *** 05/16/07 ***********************************************
     C                   MOVE      'B'           @AHBAF            1
     C                   EXSR      ADDREC
     C*** END   ADD *** 05/16/07 ***********************************************
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C*
     C                   MOVEL     DSTIME        ACTIME
     C*
     C                   UPDATE    DBRCDF
     C*** BEGIN ADD *** 05/16/07 ***********************************************
     C                   MOVE      'A'           @AHBAF            1
     C                   EXSR      ADDREC
     C*** END   ADD *** 05/16/07 ***********************************************
     C*** BEGIN ADD ***  12/27/11  ************************************
     C     ACPNAM        IFNE      SVPNAM
     C                   CALL      'WA085R'
     C                   PARM      'D'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C                   ENDIF
     C*** END ADD *****  12/27/11  ************************************
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C*
     C                   MOVEL     DSTIME        ACTIME
     C                   MOVEL     Q1USER        ACIUSR
     C                   MOVEL     Q1PGM         ACIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACIDT
     C*
     C                   WRITE     DBRCDF
     C*** BEGIN ADD *** 05/16/07 ***********************************************
     C                   MOVE      'B'           @AHBAF
     C                   EXSR      ADDREC
     C*** END   ADD *** 05/16/07 ***********************************************
     C*
     C                   FEOD      WDACP
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/16/07 ***********************************************
     C*================================================================
     C* ADDREC - Store the Image in WDAHP
     C*================================================================
     C     ADDREC        BEGSR
      *
     C                   CLEAR                   PAH01
     C                   MOVE      ACCO#         AHCO#
     C                   MOVE      ACFND         AHFND
     C                   MOVE      ACFYR         AHFYR
     C                   MOVE      ACCASE        AHCASE
     C                   MOVE      @AHBAF        AHBAFL
     C                   MOVEL     ACPNAM        AHLSN
     C                   MOVE      ACPNAM        @AHFSN           10
     C                   MOVEL     @AHFSN        AHFSN
     C                   MOVE      ACADR1        AHADR1
     C                   MOVE      ACADR2        AHADR2
     C                   MOVE      ACCTY         AHCTY
     C                   MOVE      ACST          AHST
     C                   MOVE      ACZIP         AHZIP
     C                   MOVE      ACCTRY        AHCTRY
     C                   MOVE      AHADR3        AHADR3
     C                   MOVE      ACCKAL        AHCKAL
     C                   MOVE      Q1PGM         AHPGM
     C                   MOVE      Q1USER        AHUSER
     C                   MOVE      @AHTIM        AHTIME
     C                   MOVE      @@AHDT        AHDT
     C                   WRITE     PAH01
     C*
     C                   ENDSR
     C*** END   ADD *** 05/16/07 ***********************************************
     C*** BEG ADD ***** 07/14/09 *************************************
     C*===============================================================
     C*   AUDIT  WRITE DART IF CLAIMANT INFO CHANGES
     C*===============================================================
     C*
     C     AUDIT         BEGSR
      *
     C                   MOVE      ' '           @@AFL             1
     C     SCPNAM        IFNE      SVPNAM
     C     SVPNAM        ANDNE     *BLANK
     C*** BEG ADD *** 05/30/18 ****************************
     C     SCNAMF        ORNE      SVNAMF
     C     SVNAMF        ANDNE     *BLANK
     C*** END ADD *** 05/30/18 ****************************
     C     SCADR1        ORNE      SVADR1
     C     SVADR1        ANDNE     *BLANK
     C     SCADR2        ORNE      SVADR2
     C     SVADR2        ANDNE     *BLANK
     C     SCCTY         ORNE      SVCTY
     C     SVCTY         ANDNE     *BLANK
     C     SCST          ORNE      SVST
     C     SVST          ANDNE     *BLANK
     C     SCZIP1        ORNE      SVZIP1
     C     SVZIP1        ANDNE     *ZERO
     C     SCZIP2        ORNE      SVZIP2
     C     SVZIP2        ANDNE     *ZERO
     C*
     C                   MOVE      'Y'           @@AFL
     C*** BEG   ADD *** 10/03/11 **************************************
     C     SCADR1        IFEQ      SVADR1
     C     SCADR2        ANDEQ     SVADR2
     C     SCCTY         ANDEQ     SVCTY
     C     SCST          ANDEQ     SVST
     C     SCZIP1        ANDEQ     SVZIP1
     C     SCZIP2        ANDEQ     SVZIP2
     C*
     C                   MOVE      *BLANKS       XXCHAD          116
     C                   ELSE
     C*** BEG   ADD *** 10/20/11 **************************************
     C                   MOVE      *BLANKS       XXCHAD          116
     C*** END   ADD *** 10/20/11 **************************************
     C                   MOVE      SVZIP1        XXZIP1            5
     C                   MOVE      SVZIP2        XXZIP2            5
     C                   CAT(P)    SVADR1:1      XXCHAD
     C                   CAT(P)    SVADR2:1      XXCHAD
     C                   CAT(P)    SVCTY:1       XXCHAD
     C                   CAT(P)    SVST:1        XXCHAD
     C                   CAT(P)    XXZIP1:1      XXCHAD
     C                   CAT(P)    XXZIP2:1      XXCHAD
     C                   ENDIF
     C*** END   ADD *** 10/03/11 **************************************
     C*
     C                   CALL      'WA074R'
     C                   PARM      CMCO#         ##CO#
     C                   PARM      CMFND         ##FND
     C                   PARM      CMFYR         ##FYR             3 0
     C                   PARM      CMCASE        ##CASE
     C                   PARM      0             ##FEIN            9 0
     C                   PARM      0             ##MOD             3 0
     C                   PARM      CXAADP        ##RCOD            5
     C                   PARM      *BLANKS       ##RTN             1
     C*** BEG   ADD *** 10/03/11 **************************************
     C                   PARM      XXCHAD        ##XHAD          116
     C*** END   ADD *** 10/03/11 **************************************
     C                   ENDIF
     C*
     C                   ENDSR                                                  AUDIT
     C*** END ADD ***** 07/14/09 *************************************
