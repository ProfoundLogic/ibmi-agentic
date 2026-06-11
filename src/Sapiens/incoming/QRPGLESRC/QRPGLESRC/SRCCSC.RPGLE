     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     H/define CONVERT
     H*  API SRCCSC - User Security Verification
     H*****************************************************************
     H*  Description:
     H*
     H*    This subroutine will determine if a User has the proper security
     H*    to perform a given function or access a given field.  The input
     H*    parameters include the User Parameter Security Type field and
     H*    Security Level field as well the Client Control Security Type and
     H*    level field.
     H*    The subroutine can be executed two differenct ways:
     H*        1) Check Security Type and Level; or
     H*        2) Check Security Level Only.
     H*
     H* *  Parameters used in this program are:
     H*
     H*         SRUTYP (1 A) - User Parameter Security Type
     H*         SRULVL (3 A) - User Parameter Security Level
     H*         SRCTYP (1 A) - Client Control Security Type
     H*         SRCLVL (3 A) - Client Control Security Level
     H*         SREXEC (1 A) - Execution Type:
     H*                '1' = Check Security Type and Level
     H*                '2' = Check Security Level Only
     H*         SRRES  (1 A) - Security Check Result
     H*                '0' = "Passed" Security Check
     H*                '1' = "Did Not Pass" Security Check
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 05/16/18 C:33  Support: 70713                                 *
     H*                Ensure to compare against only uppercase       *
     H*
     H* 07/27/15 C:300 Support: PHW27659 (SUP82848)                   *
     H*               Function/option security setup for Endorsement  *
     H*               forms and Certificate Holders                   *
     H*               Use single security level for all security types*
     H*
     H* 07/26/13 C:279 Project: WC2197                                *
     H*               Added 4 new parms for additional security level *
     H*               and 4 new parms for additional security type to *
     H*               be passed into this pgm to validate the user's  *
     H*               security level.                                 *
     H*                                                               *
     H* 11/02/12 C:279 Project: Transform                             *
     H*               Corrected q1prms condition for newly added      *
     H*               entry parameters.                               *
     H*                                                               *
     H* 10/31/12 C:191 Project: Transform                             *
     H*               Added five new parms for additional security    *
     H*               levels to be passed into this pgm to validate   *
     H*               the user's security level.                      *
     H*                                                               *
     H* 04/27/98 C:15 Project: WC577                                  *
     H*               Added five new parms for additional security    *
     H*               types to be passed into this pgm to validate    *
     H*               the user's security type.                       *
     H*               (** Re-wrote pgm **)                            *
     H*                                                               *
     H* 10/07/97 C:15 PROJECT: WC577                                  *
     H*               Fixed pgm.                                      *
     H*               1) If the user sec type was a special character *
     H*                  (# $ % &) and was equal to the control sec   *
     H*                  type, this pgm would have returned an error. *
     H*               2) If the user sec type was not '*', '#' or '$' *
     H*                  and the control sec type was '#' or '$', the *
     H*                  sec type edit would have been passed.        *
     H*****************************************************************
     F*

     D*
     D*
     D****** Beg Del ***** 07/26/13 *********************************
     D*********        DS
     D**TP*****                1      6
     D*********                            DIM(6)
     D**XXTP01*                1      1
     D**XXTP02*                2      2
     D**XXTP03*                3      3
     D**XXTP04*                4      4
     D**XXTP05*                5      5
     D**XXTP06*                6      6
     C*** BEGIN ADD ****** 10/31/11 ************************************
     D*********        DS
     D**LV*****                1     18
     D*********                            DIM(6)
     D**XXLV01*                1      3
     D**XXLV02*                4      6
     D**XXLV03*                7      9
     D**XXLV04*               10     12
     D**XXLV05*               13     15
     D**XXLV06*               16     18
     D*** END   ADD ****** 10/31/11 ************************************
     D****** End Del ***** 07/26/13 *********************************
     D***  Begin Add  ***  05/23/14  **************************************
     D#ofSec           c                   10
     D***  End   Add  ***  05/23/14  **************************************
     D****** Beg Add ***** 07/26/13 *********************************
     D***  Begin Chg  ***  05/23/14  **************************************
     D**********       ds
     D**tp******               1     10
     D**********                           dim(10)
     DdsType           ds                  inz
     D  tp                     1     10    dim(#ofSec)
     D***  End   Chg  ***  05/23/14  **************************************
     D  xxTp01                 1      1
     D  xxTp02                 2      2
     D  xxTp03                 3      3
     D  xxTp04                 4      4
     D  xxTp05                 5      5
     D  xxTp06                 6      6
     D  xxTp07                 7      7
     D  xxTp08                 8      8
     D  xxTp09                 9      9
     D  xxTp10                10     10

     D***  Begin Chg  ***  05/23/14  **************************************
     D**********       ds
     D**lv******               1     30
     D**********                           dim(10)
     DdsLevel          ds                  inz
     D  lv                     1     30    dim(#ofSec)
     D***  End   Chg  ***  05/23/14  **************************************
     D  xxLv01                 1      3
     D  xxLv02                 4      6
     D  xxLv03                 7      9
     D  xxLv04                10     12
     D  xxLv05                13     15
     D  xxLv06                16     18
     D  xxLv07                19     21
     D  xxLv08                22     24
     D  xxLv09                25     27
     D  xxLv10                28     30
     D****** End Add ***** 07/26/13 *********************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D***  Begin Add  ***  05/16/18  ***********************************
      /copy sPrString
     D***  End   Add  ***  05/16/18  ***********************************

     C**************************************************************************
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRUTYP            1            User Type       1
     C                   PARM                    SRULVL            3            User Level      2
     C                   PARM                    SRCTYP            1            Client Type     3
     C                   PARM                    SRCLVL            3            Client Level    4
     C                   PARM                    SREXEC            1            Execution Type  5
     C                   PARM                    SRRES             1            Result Flag     6
     C*
     C                   PARM                    SRCTP2            1                            7
     C                   PARM                    SRCTP3            1                            8
     C                   PARM                    SRCTP4            1                            9
     C                   PARM                    SRCTP5            1                           10
     C                   PARM                    SRCTP6            1                           11
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C                   PARM                    SRCLV2            3                           12
     C                   PARM                    SRCLV3            3                           13
     C                   PARM                    SRCLV4            3                           14
     C                   PARM                    SRCLV5            3                           15
     C                   PARM                    SRCLV6            3                           16
     C*** END   ADD ****** 10/31/11 ************************************
     C****** Beg Add ***** 07/26/13 *********************************
     C                   Parm                    srCTp7            1                           17
     C                   Parm                    srCTp8            1                           18
     C                   Parm                    srCTp9            1                           19
     C                   Parm                    srCTp0            1                           20
     C                   Parm                    srCLv7            3                           21
     C                   Parm                    srCLv8            3                           22
     C                   Parm                    srCLv9            3                           23
     C                   Parm                    srCLv0            3                           24
     C****** End Add ***** 07/26/13 *********************************
     C*
     C*  Initialize fields
     C*
     C                   MOVE      *ON           SRRES                          No Access
     C                   MOVE      'N'           CHKLVL            1
     C                   Z-ADD     0             X                 3 0
     C*
     C*  Determine the # of security types passed into the pgm, then
     C*    load them into an array.
     C*
     C                   MOVE      SRCTYP        XXTP01
     C     Q1PRMS        IFGT      6
     C                   MOVE      SRCTP2        XXTP02
     C                   MOVE      SRCTP3        XXTP03
     C                   MOVE      SRCTP4        XXTP04
     C                   MOVE      SRCTP5        XXTP05
     C                   MOVE      SRCTP6        XXTP06
     C                   ENDIF
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C*  Determine the # of security level passed into the pgm, then
     C*    load them into an array.
     C                   MOVE      SRCLVL        XXLV01
     C     Q1PRMS        IFGT      11
     C                   MOVE      SRCLV2        XXLV02
     C                   MOVE      SRCLV3        XXLV03
     C                   MOVE      SRCLV4        XXLV04
     C                   MOVE      SRCLV5        XXLV05
     C                   MOVE      SRCLV6        XXLV06
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************
     C****** Beg Add ***** 07/27/15 *********************************
      /free
       If %parms >= 7 and %parms <= 11;
          xxLv02 = srClvl;
          xxLv03 = srClvl;
          xxLv04 = srClvl;
          xxLv05 = srClvl;
          xxLv06 = srClvl;
       Endif;
      /end-free
     C****** End Add ***** 07/27/15 *********************************
     C****** Beg Add ***** 07/26/13 *********************************
      /free
       If %parms >= 24;
          xxTp07 = srCTp7;
          xxTp08 = srCTp8;
          xxTp09 = srCTp9;
          xxTp10 = srCTp0;
          xxLv07 = srCLv7;
          xxLv08 = srCLv8;
          xxLv09 = srCLv9;
          xxLv10 = srCLv0;
       Endif;
       //***  Begin Add  ***  05/16/18  ***********************************
       For x = 1 to #ofSec;
          tp(x) = Upper(tp(x));
       Endfor;
       Clear x;
       srUTyp = Upper(srUTyp);
       //***  End   Add  ***  05/16/18  ***********************************
      /end-free
     C****** End Add ***** 07/26/13 *********************************
     C*
     C*  If only checking the user's security level or the user's
     C*    security type is '*' or no security types were passed into
     C*    the pgm, skip the security type verification.
     C*
     C****** Beg Chg ***** 07/26/13 *********************************
     C*****SREXEC        IFEQ      '2'
     C*****SRUTYP        OREQ      '*'
     C*****TP(1)         OREQ      *BLANK
     C*****TP(2)         ANDEQ     *BLANK
     C*****TP(3)         ANDEQ     *BLANK
     C*****TP(4)         ANDEQ     *BLANK
     C*****TP(5)         ANDEQ     *BLANK
     C*****TP(6)         ANDEQ     *BLANK
      /free
       //  Begin Chg  ***  05/23/14  **************************************
       //If srExec = '2' or srUTyp = '*' or (tp(1) = *blank and tp(2) = *blank
       //and tp(3) = *blank and tp(4) = *blank and tp(5) = *blank
       //and tp(6) = *blank and tp(7) = *blank and tp(8) = *blank
       //and tp(9) = *blank and tp(10) = *blank);
       If srExec = '2' or srUTyp = '*' or dsType = *blank;
       //  End   Chg  ***  05/23/14  **************************************
      /end-free
     C****** End Chg ***** 07/26/13 *********************************
     C                   MOVE      'Y'           CHKLVL
     C                   GOTO      LVLTAG
     C                   ENDIF
     C*
     C*  Loop thru the array to process all of the security types.
     C*
     C****** Beg Chg ***** 07/26/13 *********************************
B1   C*****              DO        6             X
      /free
       //  Begin Chg  ***  05/23/14  **************************************
       //Dow x < 10;
       //  x = x + 1;
       For x = 1 to #ofSec;
       //  End   Chg  ***  05/23/14  **************************************
      /end-free
     C****** End Chg ***** 07/26/13 *********************************
     C*
B2   C     TP(X)         IFNE      *BLANK
     C*
B3   C                   SELECT
     C*
     C*  User Parm Sec Type is equal to Client Control Sec Type
     C*
     C     SRUTYP        WHENEQ    TP(X)
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C     SRULVL        IFNE      *BLANKS
     C     SRULVL        ANDNE     *ZEROS
     C     SRULVL        ANDGE     LV(X)
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************
     C                   MOVE      'Y'           CHKLVL
     C                   LEAVE
     C*
     C*  Type '#' allows access to Security Types "A" thru "M"
     C*
     C     SRUTYP        WHENEQ    '#'
     C     TP(X)         ANDGE     'A'
     C     TP(X)         ANDLE     'M'
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C     SRULVL        IFNE      *BLANKS
     C     SRULVL        ANDNE     *ZEROS
     C     SRULVL        ANDGE     LV(X)
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************
     C                   MOVE      'Y'           CHKLVL
     C                   LEAVE
     C*
     C*  Type '$' allows access to Security Types "N" thru "Z"
     C*
     C     SRUTYP        WHENEQ    '$'
     C     TP(X)         ANDGE     'N'
     C     TP(X)         ANDLE     'Z'
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C     SRULVL        IFNE      *BLANKS
     C     SRULVL        ANDNE     *ZEROS
     C     SRULVL        ANDGE     LV(X)
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************
     C                   MOVE      'Y'           CHKLVL
     C                   LEAVE
     C*
     C*  Type '&' allows access to Security Types "0" - "9", "#", "$".
     C*
     C     SRUTYP        WHENEQ    '&'
     C     TP(X)         IFGE      'A'
     C     TP(X)         ANDLE     'Z'
     C     TP(X)         OREQ      '#'
     C     TP(X)         OREQ      '$'
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C     SRULVL        IFNE      *BLANKS
     C     SRULVL        ANDNE     *ZEROS
     C     SRULVL        ANDGE     LV(X)
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************
     C                   MOVE      'Y'           CHKLVL
     C                   LEAVE
     C                   END
     C*
     C*  Type '%' allows access to Security Types "0" thru "9"
     C     SRUTYP        WHENEQ    '%'
     C     TP(X)         ANDGE     '0'
     C     TP(X)         ANDLE     '9'
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C     SRULVL        IFNE      *BLANKS
     C     SRULVL        ANDNE     *ZEROS
     C     SRULVL        ANDGE     LV(X)
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************
     C                   MOVE      'Y'           CHKLVL
     C                   LEAVE
     C*
E3   C                   ENDSL
     C*
E2   C                   ENDIF                                                  TP,X IFNE *BLANK
     C*
       //  Begin Chg  ***  05/23/14  **************************************
E1   C**********         ENDDO                                                  DO  6
      /Free
       Endfor;
      /End-free
       //  End   Chg  ***  05/23/14  **************************************
     C*
     C     LVLTAG        TAG
     C*
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C* If type was matched, X > 0
     C     CHKLVL        IFEQ      'Y'
     C     X             ANDGT     0
     C     SRULVL        ANDNE     *BLANKS
     C     SRULVL        ANDNE     *ZEROS
     C     SRULVL        ANDGE     LV(X)
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*** END   ADD ****** 10/31/11 ************************************

     C*  If CHKLVL is assigned 'Y' from one of the above conditions,
     C*  andn if the User Parm Sec Level is greater than or equal to
     C*  the Client Control Sec Level, then assign '0' to the Access
     C*  Result Flag.
     C*
      *
     C     CHKLVL        IFEQ      'Y'
     C*** BEGIN ADD ****** 10/31/11 ************************************
     C     X             ANDEQ     0
     C*** END   ADD ****** 10/31/11 ************************************
     C     SRULVL        ANDGE     SRCLVL
     C     SRRES         ANDNE     *OFF
     C                   MOVE      *OFF          SRRES                          Access
     C                   ENDIF
     C*
     C*  End Program
     C*
     C                   MOVE      '1'           *INLR
