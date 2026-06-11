     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SREPT1 - CALCULATE EARNED PREMIUMS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM CALCULATES EARNED PREMIUMS FOR A DATE RANGE    *
     H*   AND FOR THE CURRENT MONTH.  FOR COMMON EXPIRATION DATE      *
     H*   FUNDS, THE DATE RANGE IS THE START OF THE FUND YEAR THROUGH *
     H*   THE REQUESTED DATE.  FOR NON-COMMON EXPIRATION DATE FUNDS,  *
     H*   THE DATE RANGE IS PASSED IN.  ANNUAL, NORMAL, AND STANDARD  *
     H*   EARNED PREMIUMS ARE CALCULATED.                             *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   REQUESTING PROGRAMS                                         *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 08/29/95               PROJECT: WC311                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/07/02 C:65 PROJECT WC1055                                  *
     H*               REPLACE THE EXPENSE CONSTANT CHECK ON STATE     *
     H*               REPORTING DIVISION WITH A CHECK ON A VALUE.     *
     H*                                                               *
     H* 02/07/02 C:65 PROJECT WC1067                                  *
     H*               BREAK OUT EXPENSE CONSTANT ON A MIN PREM POLICY *
     H*               IN A/R IF INDICATED.                            *
     H*                                                               *
     H* 11/27/01  C:65  PROJECT WC1059                                *
     H*                 PULL EXPENSE CONSTANT FROM NEW FIELD.         *
     H*                                                               *
     H* 10/13/99  C:65  PROJECT WC942                                 *
     H*                 EXCLUDE FLAT CANCELLED DIVISIONS WHEN         *
     H*                 CHECKING FOR HIGHEST EXPENSE CONSTANT         *
     H*                                                               *
     H* 01/19/99  C:09  PROJ: SUPP (WA048/SEQ# 14944)                 *
     H*                 MODIFY PROGRAM TO PROPERLY RECOGNIZE THE      *
     H*                 CENTURY DIGIT FOR YEAR 2000 PROCESSING.       *
     H*                                                               *
     H* 06/04/98  C:09  PROJ: SUPP (SEQ# 11561)                       *
     H*                 EXPENSE CONSTANT IS NOW BY EFF DT AND STATE   *
     H*                 MODIFY PROCESSING TO RETRIEVE VALUE EXPENSE   *
     H*                 CONSTANT DEFINITION FILE (WDECP).             *
     H*                                                               *
     H* 04/03/98  C:35  PROJECT: SUPPORT                              *
     H*                 CORRECTED LEAP DATE LOGIC SO THAT SRCVTDAT IS *
     H*                 ONLY CALLED ONCE FOR PERFORMANCE REASONS.     *
     H*                                                               *
     H* 03/17/98  C:87  PROJ: SUPP (SEQ# 10676)                       *
     H*                 CORRECTED X1FRDT TO 8 DIGIT FIELD. WAS        *
     H*                 CAUSING ERROR IN CALC OF "PREM TO DATE"       *
     H*                                                               *
     H* 12/15/97  C:09  PROJ: WC577                                   *
     H*                 ENSURE PROPER CALCULATION FOR SHORT-TERM      *
     H*                 COMMERCIAL POLICIES.                          *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  50  CONTROL A/R READ LOOP                                    *
     H*  60  UTILITY INDICATOR                                        *
     H*****************************************************************
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEAL6    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
      ****** BEGIN ADD ***** 06/04/98 ********************************
     F*** BEGIN DEL ***  11/27/01  ************************************
     F*****WDELL1  IF  E           K        DISK
     F*****            PEL01                             KRENAMEPEL02
     F*****WDECL   IF  E           K        DISK
     F*****WMRSL   IF  E           K        DISK
     F*** END DEL *****  11/27/01  ************************************
     F*** BEGIN ADD ***  02/07/02  ************************************
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
     F*** END ADD *****  02/07/02  ************************************
      ****** END ADD ******* 06/04/98 ********************************

     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)            DAYS IN MONTHS
     D*
     D                 DS
     D  XXFRDT                 1      7  0
      ****** BEGIN CHG ***** 01/19/99 ********************************
     D****************                        2   30XXFRYR
     D  XXFRCY                 1      3  0
      ****** END CHG ******* 01/19/99 ********************************
     D  XXFRMO                 4      5  0
     D  XXFRDY                 6      7  0
     D  XXFRYM                 1      5  0
     D*
     D                 DS
     D  XXTODT                 1      7  0
      ****** BEGIN CHG ***** 01/19/99 ********************************
     D****************                        2   30XXTOYR
     D  XXTOCY                 1      3  0
      ****** END CHG ******* 01/19/99 ********************************
     D  XXTOMO                 4      5  0
     D  XXTODY                 6      7  0
     D  XXTOYM                 1      5  0
     D*
     D                 DS
     D  XXWKDT                 1      7  0
      ****** BEGIN CHG ***** 01/19/99 ********************************
     D****************                        2   30XXWKYR
     D  XXWKCY                 1      3  0
      ****** END CHG ******* 01/19/99 ********************************
     D  XXWKMO                 4      5  0
     D  XXWKDY                 6      7  0
     D  XXWKYM                 1      5  0
     D*
     D                 DS
     D  ELPFDT                 1      7  0
      ****** BEGIN CHG ***** 01/19/99 ********************************
     D****************                        2   30XXPFYR
     D  XXPFCY                 1      3  0
      ****** END CHG ******* 01/19/99 ********************************
     D  XXPFMO                 4      5  0
     D  XXPFDY                 6      7  0
     D  XXPFYM                 1      5  0
     D*
     D                 DS
     D  XXPTDT                 1      7  0
      ****** BEGIN CHG ***** 01/19/99 ********************************
     D****************                        2   30XXPTYR
     D  XXPTCY                 1      3  0
      ****** END CHG ******* 01/19/99 ********************************
     D  XXPTMO                 4      5  0
     D  XXPTDY                 6      7  0
     D  XXPTYM                 1      5  0
     D*
     D                 DS
     D  EAPODT                 1      7  0
      ****** BEGIN CHG ***** 01/19/99 ********************************
     D****************                        2   30XXPOYR
     D  XXPOCY                 1      3  0
      ****** END CHG ******* 01/19/99 ********************************
     D  XXPOMO                 4      5  0
     D  XXPODY                 6      7  0
     D  XXPOYM                 1      5  0
     D*
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0

     C*
     C* EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C* FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* BILLING SCHEDULE TABLE FILE
     C*
     C     BSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWK10           10
     C*
     C* TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C* A/R BILLING RECORDS
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXWKDT
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      ****** BEGIN ADD ***** 06/04/98 ********************************
     C*
     C* KEY LISTS FOR THE EXPENSE CONSTANT DEFINITION FILE
     C*
     C*** BEGIN DEL ***  11/27/01  ************************************
     C*********  ECKY      KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXST
     C*********            KFLD           XXEFDT
      *
     C*********  ECKY1     KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXST
     C*** END DEL *****  11/27/01  ************************************
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
      ****** END ADD ******* 06/04/98 ********************************
     C*** BEGIN ADD ***  02/07/02  ************************************
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END ADD *****  02/07/02  ************************************
     C*
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EADT          XXDATE
     C     *LIKE         DEFINE    EADT          XQFROM
     C     *LIKE         DEFINE    EADT          XQTO
      ****** BEGIN ADD ***** 06/04/98 ********************************
     C*** BEGIN CHG ***  11/27/01  ************************************
     C*********  *LIKE     DEFN ECEXCO    XXEXCO
     C     *LIKE         DEFINE    ELPRTF        XXEXCO
     C*** END CHG *****  11/27/01  ************************************
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
      ****** END ADD ******* 06/04/98 ********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRCO#             3
     C                   PARM                    SRFND             3
     C                   PARM                    SRFYR             3
     C                   PARM                    SREMP#            9
     C                   PARM                    SRDIV             5
     C                   PARM                    SRDATE            8            COMES IN M/D/Y
     C                   PARM                    SRFRDT            8            COMES IN M/D/Y
     C                   PARM                    SRTODT            8            COMES IN M/D/Y
     C                   PARM                    SRRNGE            1            DATE RANGE IF 'Y'
     C                   PARM                    SRANN            11            ANN ERN PRM
     C                   PARM                    SRANNM           11            ANN ERN PRM 1 MTH
     C                   PARM                    SRNRM            11            NRM ERN PRM
     C                   PARM                    SRNRMM           11            NRM ERN PRM 1 MTH
     C                   PARM                    SRSTD            11            STD ERN PRM
     C                   PARM                    SRSTDM           11            STD ERN PRM 1 MTH
     C                   PARM                    SRERR             1            RETURN *ON IF ERR
     C                   PARM                    SRTERM            1            SET LR FLAG.
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRCO#         XXCO#             3 0
     C                   MOVE      SRFND         XXFND             3 0
     C                   MOVE      SRFYR         XXFYR             3 0
     C                   MOVE      SREMP#        XXEMP#
     C                   MOVE      SRDIV         XXDIV
     C                   MOVE      SRDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  RETRIEVE THE FUND DETAIL, EMPLOYER MASTER, AND EMPLOYER DETAIL
     C*  FOR THE REQUESTED ACCOUNT.  IF ANY OF THESE RECORDS ARE
     C*  MISSING, SET ON THE ERROR FLAG AND EXIT.
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C  N60EMKY          CHAIN     PEM01                              60
     C*
     C  N60ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         SRANN
     C                   MOVE      *ZERO         SRANNM
     C                   MOVE      *ZERO         SRNRM
     C                   MOVE      *ZERO         SRNRMM
     C                   MOVE      *ZERO         SRSTD
     C                   MOVE      *ZERO         SRSTDM
     C                   MOVE      *ON           SRERR
     C                   GOTO      RETURN
     C                   ENDIF
     C*
     C*  ESTABLISH THE DATE RANGE TO USE FOR EARNED PREMIUMS
     C*
     C     SRRNGE        IFEQ      'Y'
      ****** BEGIN ADD ***** 03/17/98 ********************************
     C******************** MOVE SRFRDT    X1FRDT  60
     C                   MOVE      SRFRDT        X1FRDT            8 0
      ****** END   ADD ***** 03/17/98 ********************************
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      SRTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      ****** BEGIN ADD ***** 12/15/97 *********************************
     C*
     C*  IF THIS IS A NON-COMMON EXPIRATION DATE POLICY,
     C*  FUND DATES WILL BE ELPFDT AND ELPTDT
     C*
     C                   Z-ADD     ELPFDT        F1STPO
     C                   Z-ADD     ELPTDT        F1ENPO
      ****** END ADD ******* 12/15/97 *********************************
     C*
     C                   ELSE
      ****** BEGIN ADD ***** 12/15/97 *********************************
     C*
     C*  DETERMINE OUR DATES, USE THE FUND POLICY FROM DATE
     C*  AND THE FUND POLICY TO DATE, OR THE POLICY DATES FROM
     C*  THE EMPLOYER DETAIL FILE
     C*
     C     F1STPO        IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        F1STPO
     C                   END
     C*
     C     F1ENPO        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        F1ENPO
     C                   END
      ****** END ADD ******* 12/15/97 *********************************
     C*
     C                   Z-ADD     F1STPO        XXFRDT
     C                   Z-ADD     XXDATE        XXTODT
     C                   ENDIF
     C*
     C*  CHECK IF EMPLOYER IS CANCELLED
     C*
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   Z-ADD     5             Y                 3 0
     C*
     C     Y             DOWNE     *ZERO
     C*
     C     ED(Y)         IFEQ      *ZERO
     C     ED(Y)         ORNE      *ZERO
     C     ELPTDT        ANDLE     ED(Y)
     C                   SUB       1             Y
     C*
     C                   ELSE
     C*
     C     CD(Y)         IFNE      *ZERO
     C     CD(Y)         ANDLT     ELPTDT
     C                   Z-ADD     CD(Y)         XXPTDT
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         Y
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  IF THE POLICY DATE RANGE FALLS OUTSIDE THE REQUESTED DATE RANGE,
     C*  NO PREMIUM WILL BE EARNED.
     C*
     C     ELPFDT        IFGT      XXTODT
     C     XXPTDT        ORLE      XXFRDT
     C                   MOVE      *ZERO         SRANN
     C                   MOVE      *ZERO         SRANNM
     C                   MOVE      *ZERO         SRNRM
     C                   MOVE      *ZERO         SRNRMM
     C                   MOVE      *ZERO         SRSTD
     C                   MOVE      *ZERO         SRSTDM
     C                   GOTO      RETURN
     C                   ENDIF
     C*
     C*  CALCULATE FDAY, THE NUMBER OF DAYS IN THE FUND YEAR
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM            8 0
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO              8 0
     C*
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   EXSR      SRLEAP
     C                   Z-ADD     QQDAYS        FDAY              5 0
     C*
     C*  CALCULATE PDAY, THE NUMBER OF DAYS IN THE POLICY
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   MOVE      X1FROM        QQFROM
     C                   MOVE      X1TO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   EXSR      SRLEAP
     C                   Z-ADD     QQDAYS        PDAY              5 0
     C*
     C*  IF PDAY IS ZERO, NO PREMIUM CAN BE EARNED
     C*
     C     PDAY          IFEQ      *ZERO
     C                   MOVE      *ZERO         SRANN
     C                   MOVE      *ZERO         SRANNM
     C                   MOVE      *ZERO         SRNRM
     C                   MOVE      *ZERO         SRNRMM
     C                   MOVE      *ZERO         SRSTD
     C                   MOVE      *ZERO         SRSTDM
     C                   GOTO      RETURN
     C                   ENDIF
     C*
     C*  CALCULATE EDAY, THE NUMBER OF DAYS THE POLICY IS IN
     C*  EFFECT DURING THE REQUESTED DATE RANGE
     C*
     C     ELPFDT        IFGT      XXFRDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   ENDIF
     C     XXPTDT        IFLT      XXTODT
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   ENDIF
     C*
     C                   MOVE      X1FROM        QQFROM
     C                   MOVE      X1TO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   EXSR      SRLEAP
     C                   Z-ADD     QQDAYS        EDAY              5 0
     C*
     C*  IF THE LAST DAY EFFECTIVE WAS THE "TO" DATE FROM THE REQUESTED
     C*  RANGE, ADD 1 TO EDAY SINCE THAT DAY IS INCLUDED IN EARNED
     C*  PREMIUM.
     C*
     C     XXTODT        IFLT      XXPTDT
     C                   ADD       1             EDAY
     C                   ENDIF
     C*
     C*  CALCULATE MDAY, THE NUMBER OF DAYS THE POLICY IS EFFECTIVE
     C*  IN THE LAST MONTH OF THE DATE RANGE.
     C*
     C     XXPTYM        IFLT      XXTOYM
     C                   Z-ADD     *ZERO         MDAY              5 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXTODT        XXWKDT
     C                   Z-ADD     1             XXWKDY
     C*
     C     ELPFDT        IFGT      XXWKDT
     C                   Z-ADD     ELPFDT        XXWKDT
     C                   ENDIF
     C*
     C     XXFRDT        IFGT      XXWKDT
     C                   Z-ADD     XXFRDT        XXWKDT
     C                   ENDIF
     C*
     C     XXPTDT        IFLE      XXTODT
     C     XXPTDY        SUB       XXWKDY        MDAY
     C*
     C                   ELSE
     C     XXTODY        SUB       XXWKDY        MDAY
     C                   ADD       1             MDAY
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  CALCULATE MDAY1, THE NUMBER OF DAYS THE POLICY IS EFFECTIVE
     C*  IN THE FIRST MONTH OF THE DATE RANGE.
     C*
     C     XXPFYM        IFGT      XXFRYM
     C                   Z-ADD     *ZERO         MDAY1             5 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXFRDT        XXWKDT
     C                   Z-ADD     XXFRMO        M                 3 0
     C                   Z-ADD     MD(M)         XXWKDY
     C*
     C     XXTODT        IFLT      XXWKDT
     C                   Z-ADD     XXTODT        XXWKDT
     C                   ENDIF
     C*
     C     XXPTDT        IFLE      XXWKDT
     C                   Z-ADD     XXPTDT        XXWKDT
     C                   SUB       1             XXWKDY
     C                   ENDIF
     C*
     C     ELPFDT        IFGT      XXFRDT
     C     XXWKDY        SUB       XXPFDY        MDAY1
     C                   ADD       1             MDAY1
     C*
     C                   ELSE
     C     XXWKDY        SUB       XXFRDY        MDAY1
     C                   ADD       1             MDAY1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  FOR AUDITED ACCOUNTS, EARN PREMIUM BASED ON AUDITED AMOUNTS
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C*  SUBTRACT EXPENSE CONSTANT FROM ANNUAL FOR NORMAL
     C*
     C     ELAAPR        SUB       ELPRTD        ELAZPR           11 2
     C*
     C*  CALCULATE EARNED PREMIUMS
     C*
     C     EDAY          DIV(H)    PDAY          XXPRO            11 9
     C*
     C     ELAAPR        MULT(H)   XXPRO         XXANN            11 2
     C                   MOVE      XXANN         SRANN
     C     ELAZPR        MULT(H)   XXPRO         XXNRM            11 2
     C                   MOVE      XXNRM         SRNRM
     C     ELASPR        MULT(H)   XXPRO         XXSTD            11 2
     C                   MOVE      XXSTD         SRSTD
     C*
     C*  CALCULATE CURRENT MONTH EARNED PREMIUMS
     C*
     C     MDAY          DIV(H)    PDAY          XXPRO
     C*
     C     ELAAPR        MULT(H)   XXPRO         XXANN
     C                   MOVE      XXANN         SRANNM
     C     ELAZPR        MULT(H)   XXPRO         XXNRM
     C                   MOVE      XXNRM         SRNRMM
     C     ELASPR        MULT(H)   XXPRO         XXSTD
     C                   MOVE      XXSTD         SRSTDM
     C*
     C                   ELSE                                                   ELFAF EQ 'A'
     C*
     C*  FOR UNAUDITED ACCOUNTS, FDAY CANNOT BE ZERO
     C*
     C     FDAY          IFEQ      *ZERO
     C                   MOVE      *ZERO         SRANN
     C                   MOVE      *ZERO         SRANNM
     C                   MOVE      *ZERO         SRNRM
     C                   MOVE      *ZERO         SRNRMM
     C                   MOVE      *ZERO         SRSTD
     C                   MOVE      *ZERO         SRSTDM
     C                   MOVE      *ON           SRERR
     C                   GOTO      RETURN
     C                   ENDIF
     C*
     C*  SUBTRACT EXPENSE CONSTANT FROM ANNUAL FOR NORMAL
     C*
     C*** BEGIN CHG ***  11/07/02  ************************************
     C*********  EMSTRP    IFEQ 'Y'
     C     ELPRTF        IFNE      *ZERO
     C*** END CHG *****  11/07/02  ************************************
     C     ELEMNP        ANDNE     'Y'
     C     ELANPR        ANDNE     *ZERO
     C*
      ****** BEGIN CHG ***** 06/04/98 ********************************
     C***********ELPFDT    IFGE F1XDT2
     C***********F1XDT2    ANDNE*ZERO
     C***********          Z-ADDF1EXC2    F1EXCO
     C***********          END
     C***********
     C***********ELANPR****SUB  F1EXCO    ELAXPR 112
     C*
     C*** BEGIN CHG ***  11/27/01  ************************************
     C*********            Z-ADD0         XXEXCO
     C*********            MOVE ELPFDT    XXEFDT
     C*********  ELKY2     CHAINPEL02                60
     C*********  *IN60     DOWEQ*OFF
      *
     C*********            MOVE ELRTCD    XXRTCD
     C*********  RSKY      CHAINPRS01                60
     C*********            MOVE RSJRST    XXST    2
     C*********  ECKY      SETGTPEC01
     C*********  ECKY1     REDPEPEC01                    60
      *
     C*********  *IN60     IFEQ *OFF
     C*********  ECEXCO    ANDGTXXEXCO
     C*** BEGIN ADD ***  10/13/99  ************************************
     C*********  ELPFDT    ANDNEELPTDT
     C*** END ADD *****  10/13/99  ************************************
     C*********            Z-ADDECEXCO    XXEXCO
     C*********            ENDIF                           *IN60 IFEQ
     C*********  ELKY2     READEPEL02                    60
     C*********            ENDDO                           *IN60 DOWEQ
     C*
     C*********  ELKY      CHAINPEL01                60
     C                   Z-ADD     ELPRTF        XXEXCO
     C*** END CHG *****  11/27/01  ************************************
     C     ELANPR        SUB       XXEXCO        ELAXPR           11 2
     C*
      ****** END CHG ******* 06/04/98 ********************************
     C*
     C                   ELSE
     C                   Z-ADD     ELANPR        ELAXPR
     C*** BEGIN ADD ***  02/07/02  ************************************
     C     FNKEY         CHAIN     PEX02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ELEMNP        ANDEQ     'Y'
     C     EXBEXP        ANDEQ     'Y'
     C     ELANPR        ANDNE     *ZERO
     C     ELANPR        SUB       ELPRTF        ELAXPR           11 2
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  02/07/02  ************************************
     C                   ENDIF
     C*
     C*  RETRIEVE THE BILLING SCHEDULE TO DETERMINE IF ESTIMATING
     C*  OR SELF-REPORTING.  IF IT IS NOT FOUND, RETURN AN ERROR.
     C*
     C                   MOVEL     ELBSCD        XXWK10
     C     BSKY          CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         SRANN
     C                   MOVE      *ZERO         SRANNM
     C                   MOVE      *ZERO         SRNRM
     C                   MOVE      *ZERO         SRNRMM
     C                   MOVE      *ZERO         SRSTD
     C                   MOVE      *ZERO         SRSTDM
     C                   MOVE      *ON           SRERR
     C                   GOTO      RETURN
     C                   ENDIF
     C*
     C     BSSEFL        IFEQ      'E'
     C*
     C*  CALCULATE EARNED PREMIUMS FOR UNAUDITED ESTIMATING ACCOUNTS
     C*
     C     EDAY          DIV(H)    FDAY          XXPRO            11 9
     C*
     C     ELANPR        MULT(H)   XXPRO         XXANN            11 2
     C                   MOVE      XXANN         SRANN
     C     ELAXPR        MULT(H)   XXPRO         XXNRM            11 2
     C                   MOVE      XXNRM         SRNRM
     C     ELSTPR        MULT(H)   XXPRO         XXSTD            11 2
     C                   MOVE      XXSTD         SRSTD
     C*
     C*  CALCULATE CURRENT MONTH EARNED PREMIUMS
     C*
     C     MDAY          DIV(H)    FDAY          XXPRO
     C*
     C     ELANPR        MULT(H)   XXPRO         XXANN
     C                   MOVE      XXANN         SRANNM
     C     ELAXPR        MULT(H)   XXPRO         XXNRM
     C                   MOVE      XXNRM         SRNRMM
     C     ELSTPR        MULT(H)   XXPRO         XXSTD
     C                   MOVE      XXSTD         SRSTDM
     C*
     C                   ELSE                                                   BSSEFL EQ 'E'
     C*
     C                   CLEAR                   XXAXPR
     C                   CLEAR                   XXAXPM
     C*
     C*  FOR UNAUDITED SELF REPORTING ACCOUNTS, CHECK EACH MONTH
     C*  FOR BILLING RECORDS IN A/R.  MISSING MONTHS ARE ESTIMATED.
     C*
     C     ELPFDT        IFGT      XXFRDT
     C                   Z-ADD     ELPFDT        XXWKDT
     C                   ELSE
     C                   Z-ADD     XXFRDT        XXWKDT
     C                   ENDIF
     C*
     C                   Z-ADD     1             XXWKDY
     C*
     C     XXWKDT        DOWLE     XXTODT
     C     XXWKDT        ANDLT     XXPTDT
     C*
     C*  READ ANY BILLING OR ADJUSTMENT RECORDS FOR THE MONTH
     C*
     C                   MOVE      *OFF          XXREAD            1
     C     EAKY          SETLL     PEA01
     C     EAKY1         READE     PEA01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C*  IF THE POST DATE IS BEYOND THE CURRENT MONTH, EXIT THE LOOP
     C*
     C     XXPOYM        IFGT      XXWKYM
     C                   MOVE      *ON           *IN50
     C                   ELSE
     C*
     C*  PROCESS BILLINGS AND ADJUSTMENTS, EXCEPT EXPENSE CONSTANT BILLINGS
     C*
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     EAARTY        IFEQ      'B'
     C     TTCTYP        ANDNE     'E'
     C     EAARTY        OREQ      'A'
     C     TTCTYP        ANDNE     'E'
     C*
     C*  CALCULATE THE PRORATA FACTOR FOR THE MONTH
     C*
     C     XXREAD        IFEQ      *OFF
     C                   Z-ADD     XXWKMO        M
     C*
     C     XXWKYM        IFEQ      XXFRYM
     C     MDAY1         DIV(H)    MD(M)         XXPRO
     C*
     C                   ELSE
     C*
     C     XXWKYM        IFEQ      XXTOYM
     C     MDAY          DIV(H)    MD(M)         XXPRO
     C*
     C                   ELSE
     C                   Z-ADD     1             XXPRO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  IF THE POLICY STARTS OR ENDS DURING THE MONTH, ADJUST
     C*  THE PRORATA FACTOR
     C*
     C     XXWKYM        IFEQ      XXPFYM
     C     XXWKYM        ANDNE     XXPTYM
     C*
     C     XXFRDY        IFLE      XXPFDY
     C                   Z-ADD     1             XXPRO
     C*
     C                   ELSE
     C     MD(M)         SUB       XXPFDY        QQDAYS
     C                   ADD       1             QQDAYS
     C     MDAY1         DIV(H)    QQDAYS        XXPRO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XXWKYM        IFEQ      XXPTYM
     C     XXWKYM        ANDNE     XXPFYM
     C*
     C     XXFRDY        IFGE      XXPTDY
     C                   Z-ADD     1             XXPRO
     C*
     C                   ELSE
     C     XXPTDY        SUB       1             QQDAYS
     C     MDAY          DIV(H)    QQDAYS        XXPRO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XXWKYM        IFEQ      XXPFYM
     C     XXWKYM        ANDEQ     XXPTYM
     C     XXPTDY        SUB       XXPFDY        QQDAYS
     C*
     C     MDAY          IFGE      QQDAYS
     C                   Z-ADD     1             XXPRO
     C*
     C                   ELSE
     C     MDAY          DIV(H)    QQDAYS        XXPRO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXREAD = *OFF
     C*
     C*  ACCUMULATE THE PRORATED AMOUNT INTO EARNED PREMIUM
     C*
     C                   MOVE      *ON           XXREAD
     C     EAAMT         MULT(H)   XXPRO         XXAMT            11 2
     C                   ADD       XXAMT         XXAXPR           11 2
     C*
     C*  IF THIS IS THE LAST MONTH, ALSO ACCUMULATE INTO CURRENT
     C*  MONTH EARNED PREMIUM
     C*
     C     XXWKYM        IFEQ      XXTOYM
     C                   ADD       XXAMT         XXAXPM           11 2
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARTY = B OR A
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  POYM > WKYM
     C*
     C*  READ THE NEXT A/R RECORD
     C*
     C  N50EAKY1         READE     PEA01                                  50
     C*
     C                   ENDDO                                                  *IN50 DOW *OFF
     C*
     C*  IF NO A/R RECORDS WERE AVAILABLE FOR THE MONTH, ESTIMATE
     C*  THE MONTH'S PREMIUM
     C*
     C     XXREAD        IFEQ      *OFF
     C*
     C*  CALCULATE THE PRORATA FACTOR FOR THE MONTH
     C*
     C     XXWKYM        IFEQ      XXFRYM
     C     MDAY1         DIV(H)    FDAY          XXPRO
     C*
     C                   ELSE
     C*
     C     XXWKYM        IFEQ      XXTOYM
     C     MDAY          DIV(H)    FDAY          XXPRO
     C*
     C                   ELSE
     C                   Z-ADD     XXWKMO        M
     C     MD(M)         DIV(H)    FDAY          XXPRO
     C*
     C*  IF THE POLICY STARTS OR ENDS DURING THE MONTH, ADJUST
     C*  THE PRORATA FACTOR
     C*
     C     XXWKYM        IFEQ      XXPFYM
     C     XXWKYM        ANDNE     XXPTYM
     C     XXPFDY        ANDNE     1
     C     MD(M)         SUB       XXPFDY        QQDAYS
     C                   ADD       1             QQDAYS
     C     QQDAYS        DIV(H)    FDAY          XXPRO
     C                   ENDIF
     C*
     C     XXWKYM        IFEQ      XXPTYM
     C     XXWKYM        ANDNE     XXPFYM
     C     XXPTDY        SUB       1             QQDAYS
     C     QQDAYS        DIV(H)    FDAY          XXPRO
     C                   ENDIF
     C*
     C     XXWKYM        IFEQ      XXPFYM
     C     XXWKYM        ANDEQ     XXPTYM
     C     XXPTDY        SUB       XXPFDY        QQDAYS
     C     QQDAYS        DIV(H)    FDAY          XXPRO
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  ESTIMATE THE PREMIUM
     C*
     C     ELAXPR        MULT(H)   XXPRO         XXAMT
     C                   ADD       XXAMT         XXAXPR
     C*
     C     XXWKYM        IFEQ      XXTOYM
     C                   ADD       XXAMT         XXAXPM
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXREAD = *OFF
     C*
     C*  LOAD THE NEXT MONTH INTO XXWKDT
     C*
     C                   ADD       1             XXWKMO
     C*
     C     XXWKMO        IFEQ      13
      ****** BEGIN CHG ***** 01/19/99 ********************************
     C*********************ADD  1         XXWKYR
     C                   ADD       1             XXWKCY
      ****** END CHG ******* 01/19/99 ********************************
     C                   Z-ADD     1             XXWKMO
     C                   ENDIF
     C*
     C                   ENDDO                                                  WKDT DOWLT TODT
     C*
     C*  CALCULATE EARNED PREMIUMS
     C*
     C     ELANPR        SUB       ELAXPR        XXEXCO
     C*
     C     EDAY          DIV(H)    FDAY          XXPRO            11 9
     C*
     C     XXPRO         MULT(H)   XXEXCO        XXEXC1           11 2
     C     XXAXPR        ADD       XXEXC1        XXANPR           11 2
     C*
     C                   MOVE      XXANPR        SRANN
     C                   MOVE      XXAXPR        SRNRM
     C     ELSTPR        MULT(H)   XXPRO         XXSTD            11 2
     C                   MOVE      XXSTD         SRSTD
     C*
     C*  CALCULATE CURRENT MONTH EARNED PREMIUMS
     C*
     C     MDAY          DIV(H)    FDAY          XXPRO
     C*
     C     XXPRO         MULT(H)   XXEXCO        XXEXC1           11 2
     C     XXAXPM        ADD       XXEXC1        XXANPM           11 2
     C*
     C                   MOVE      XXANPM        SRANNM
     C                   MOVE      XXAXPM        SRNRMM
     C     ELSTPR        MULT(H)   XXPRO         XXSTD
     C                   MOVE      XXSTD         SRSTDM
     C*
     C                   ENDIF                                                  BSSEFL EQ 'E'
     C*
     C                   ENDIF                                                  ELFAF EQ 'A'
     C*
     C     RETURN        TAG
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      *ON           *INLR
     C                   END
     C*
     C     SRLEAP        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
      **** BEGIN CHG **** 04/03/98 *********************************************
     C******************** Z-ADD0         XXLPDT  60
     C                   Z-ADD     0             XXLPDT            8 0
      **** END   CHG **** 04/03/98 *********************************************
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X1TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
      **** BEGIN DEL **** 04/03/98 *********************************************
     C******************** Z-ADDXXLPDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     X1FROM
      **** END   DEL **** 04/03/98 *********************************************
     C*
     C                   Z-ADD     XXLPDT        SRDTI
      **** BEGIN CHG **** 04/03/98 *********************************************
     C******************** CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     X1FROM
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXLPDT
      **** END   CHG **** 04/03/98 *********************************************
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
**  MD - DAYS IN MONTHS
312831303130313130313031
