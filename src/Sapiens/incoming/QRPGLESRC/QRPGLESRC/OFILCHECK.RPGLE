      * ******************************************************************
      * COPYBOOK:  OCHKCHECK
      * ******************************************************************
      * REVISIONS:
      *
      *  10/20/16 C: 53 PROJECT 34WCA
      *                 Add Pay To info that was removed from clients
      *                 version of overlay
      *
      *  06/11/12 C:277 PROJECT 114MER
      *                 Convert from RPG to RPGLE
      *
      * 11/28/05 C:53  Project: 136WAH
      *                MOVE BANK NAME ABOVE THE CHECK #
      *
      * 06/23/05 C:53  Project: WC
      *                MOVE DATE AND CHECK # ON CHECK, UP
      *
      * 10/20/03 C:53  Project: WC909
      *                Added country info.
      *
      * 10/28/98 C:15 Project:
      *               Moved the upper and lower routing entries down due
      *               clients with border on top of the check stock.
      *
      * 02/02/98  47
      *           PRINT GROUP NAME/ADDRESS IN UPPER LEFT CORNER.
      *
      * ******************************************************************
     OFILECOPY  E            CHECK       0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *  use Lower Tray
     O                       ESC
     O                                              '&l4H'
     O                       ESC
     O                       MACRO
     O          E   N75      CHECK       0  0
      *                                                Font Change
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E   N75      CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O                                              '*p2250x120Y'
     O                       ESC
     O                                              '&d1D'
     O                       BIRTUP
     O                       ESC
     O                                              '&d@'
     O          E   N75      CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O                                              '*p2250x155Y'
     O                       BIRTLW
     O          E            CHECK       0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1850x165Y'
     O                       @@CHK#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
     O** ITALIC, NOT BOLD                   '(s0p12h10v1s0T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECK       0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x155Y'
     O                       @@DATE        Y
     O          E            CHECK       0  0
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
     O          E   N07      CHECK       0  0
      *                                       PRINT CLAIM# ON CK
     O                       ESC
     O                                              '*p0050x371Y'
     O                       XXCLM#                 '   -   -       '
     O          E   N07      CHECK       0  0
      *                                       PRINT Claimant Name
     O                       ESC
     O                                              '*p700x371Y'
     O                       @@NAME
     O          E            CHECK       0  0
      *                                       PRINT PAYEE  FEIN #
     O                       ESC
     O                                              '*p1950x371Y'
     O              N44      @@SS#
     O               44      @@FEIN
     O          E   N07      CHECK       0  0
      *                                       PRINT Accident date
     O                       ESC
     O                                              '*p400x371Y'
     O                       @@ACDT        Y
     O          E   N75      CHECK       0  0
      *                                                Font Change
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E   N75      CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1400x100Y'
     O                       BIAI01
     O          E            CHECK       0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       @@AMT                  '  ,   , $0.  '
     O*
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       @@AMT1
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       @@AMT2
      *** beg chg *** 10/20/16 **************
     O          E   N62      CHECK       0  0
     O********* E            CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p10h12v0s0b3T'
      *** beg add *** 10/20/16 **************
     O          E    62      CHECK       0  0
      *  Courier 10, using 12 point, 10 pitch  "Bitmapped" font
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p12h12v0s0b3T'
      *
     O          E    62      CHECK       0  0
     O                       ESC
     O                                              '*p050x650Y'
     O                       @@PAY1
     O          E    62      CHECK       0  0
     O                       ESC
     O                                              '*p050x700Y'
     O                       @@PAY2
      *                                           CHECK NAME/ADDRESS
     O          E    62      CHECK       0  0
     O                       ESC
     O                                              '*p370x650Y'
     O                       @@PNAM
     O          E    62      CHECK       0  0
     O                       ESC
     O                                              '*p370x700Y'
     O                       @@ADR1
     O          E   N61 62   CHECK       0  0
     O                       ESC
     O                                              '*p370x750Y'
     O                       @@ADR2
     O          E    61 62   CHECK       0  0
     O                       ESC
     O                                              '*p370x750Y'
     O                       XXCSZ
     O          E   N61 62   CHECK       0  0
     O                       ESC
     O                                              '*p370x800Y'
     O                       XXCSZ
     O          E    61 62   CHECK       0  0
     O                       ESC
     O                                              '*p370x800Y'
     O                       XXADR4
     O          E   N61 62   CHECK       0  0
     O                       ESC
     O                                              '*p370x850Y'
     O                       XXADR4
      *
      *** end add *** 10/20/16 **************
      *  Courier 10, using 12 point, 10 pitch  "Bitmapped" font
      *                                           CHECK NAME/ADDRESS
      *** beg add *** 10/20/16 **************
     O          E   N62      CHECK       0  0
      *** end add *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x650Y'
     O                       @@PNAM
      *** beg chg *** 10/20/16 **************
     O********* E            CHECK       0  0
     O          E   N62      CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x700Y'
     O                       @@ADR1
      *** beg chg *** 10/20/16 **************
     O********* E   N61      CHECK       0  0
     O          E   N61N62   CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x750Y'
     O                       @@ADR2
      *** beg chg *** 10/20/16 **************
     O********* E    61      CHECK       0  0
     O          E    61N62   CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x750Y'
     O                       XXCSZ
      *** beg chg *** 10/20/16 **************
     O********* E   N61      CHECK       0  0
     O          E   N61N62   CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x800Y'
     O                       XXCSZ
      *** beg chg *** 10/20/16 **************
     O********* E    61      CHECK       0  0
     O          E    61N62   CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x800Y'
     O                       XXADR4
      *** beg chg *** 10/20/16 **************
     O********* E   N61      CHECK       0  0
     O          E   N61N62   CHECK       0  0
      *** end chg *** 10/20/16 **************
     O                       ESC
     O                                              '*p250x850Y'
     O                       XXADR4
     O*
     O          E    85      CHECK       0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*       E 00     85      CHECK
     O*                        ESC
     O*                                     '*p1400x605Y'
     O*       E 00     85      CHECK
     O*                        SIGMS1
     O*                        BI$LMT       ' ,   ,   , $0'
     O*                        SIGMS2
     O          E    86      CHECK       0  0
     O                       ESC
     O                                              '*p1400x710Y'
     O*
     O*
     O          E            CHECK       0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1600x700Y'
     O                                              'FILE COPY'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1500x830Y'
     O                                              'NON-NEGOTIABLE'
     O*
     O          E            CHECK       0  0
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECK       0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
