      * ******************************************************************
      * COPYBOOK:  OCHKCHECK
      * ******************************************************************
      * REVISIONS:
      *
      *  03/06/19 C:180 PROJECT: 126WFR
      *                   Add dashes to Void after x days for Client 126.
      *
      *  08/21/18 C:180 PROJECT: 126WFR
      *                 Use payee's font for VOID AFTER x DAYS.
      *                 Add variable text on check.
      *
      *  06/18/18 C:062 PROJECT: 129WVB
      *                 Move the micr line to the left for CoveRisk
      *
      *  04/11/18 C:053 PROJECT: 129WVC
      *                 Special printing of lines & no signature on $ limit
      *
      *  09/26/17 C: 53 SUPPORT 46763
      *                 Correction to client 146 amount printing.
      *
      *  10/20/16 C: 53 PROJECT 34WCA
      *                 Add Pay To info that was removed from clients
      *                 version of overlay
      *
      *  01/21/16 C: 53 SUPPORT 39472
      *                 BTB Check changes for CLT065 Use *IN23 for ERS
      *                 Apply prior items: 11600-Move amount down for box
      *                                    12219-Move MICR line down for bank
      *
      *  06/11/12 C:277 PROJECT 114MER
      *                 Convert from RPG to RPGLE
      *
      *  03/25/11 C: 53 SUPPORT 75860
      *                 Check changes to MICR line for CLT146
      *                 Change *IN20 to be used for base printing
      *
      *04/11/07 C:053 SUPPORT 57936
      *                Check corrections for client 90
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
      * 01/26/98  47
      *           IF UNDERLINE FOR FIRST SIGNATURE IS NEEDED,
      *           UNCOMMENT LINES.
      * ******************************************************************
     OCHECKS    E            CHECK       0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
      *   Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O                                              '&l1H'
     O                       ESC
      *
     O                       MACRO
      ********************************************************************
      **** MICR BANK INFO *****
      *                                        MICR Font Change
     O          E   N75      CHECK       0  0
     O                       ESC
     O                                              '(8M'
     O                       ESC
     O                                              '(s0p8h8v0s0b80T'
      *
      *
      * BASE
     O          E   N75 20   CHECK       0  0
     O                       ESC
     O                                              '*p25x980Y'
     O                       XXAXOU
     O          E   N75 20   CHECK       0  0
     O                       ESC
     O                                              '*p775x980Y'
     O                       XXTRAN
     O          E   N75 20   CHECK       0  0
     O                       ESC
     O                                              '*p1225x980Y'
     O                       BIONUS
      * CLT114
     O          E   N75 21   CHECK       0  0
     O                       ESC
     O                                              '*p25x1010Y'
     O                       XXAXOU
     O          E   N75 21   CHECK       0  0
     O                       ESC
     O                                              '*p775x1010Y'
     O                       XXTRAN
     O          E   N75 21   CHECK       0  0
     O                       ESC
     O                                              '*p1225x1010Y'
     O                       BIONUS
      * CLT146
     O          E   N75 22   CHECK       0  0
     O                       ESC
     O                                              '*p15x980Y'
     O                       XXAXOU
     O          E   N75 22   CHECK       0  0
     O                       ESC
     O                                              '*p765x980Y'
     O                       XXTRAN
     O          E   N75 22   CHECK       0  0
     O                       ESC
     O                                              '*p1180x980Y'
     O                       BIONUS
      *
      *** Beg Add *** 01/21/16 *********************
      * CLT065
     O          E   N75 23   CHECK       0  0
     O                       ESC
     O                                              '*p20x1000Y'
     O                       XXAXOU
     O          E   N75 23   CHECK       0  0
     O                       ESC
     O                                              '*p770x1000Y'
     O                       XXTRAN
     O          E   N75 23   CHECK       0  0
     O                       ESC
     O                                              '*p1220x1000Y'
     O                       BIONUS
      *** END Add *** 01/21/16 *********************
      * CLT129
     O          E   N75 24   CHECK       0  0
     O                       ESC
     O                                              '*p15x980Y'
     O                       XXAXOU
     O          E   N75 24   CHECK       0  0
     O                       ESC
     O                                              '*p765x980Y'
     O                       XXTRAN
     O          E   N75 24   CHECK       0  0
     O                       ESC
     O                                              '*p1225x980Y'
     O                       BIONUS
     O          E   N75      CHECK       0  0
      *   Font 164 Times Roman Light 8pt
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
      *   Font 217 Line Printer 19Cpi
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
      *   Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E   N75      CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1400x105Y'
     O                       BIAI01
      *  Check Amount, Payee Name, and Address
     O          E            CHECK       0  0
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
      *** Beg Chg *** 01/21/16 *********************
      * BASE
     O********* E            CHECK       0  0
     O          E    20      CHECK       0  0
      *** Beg Add *** 09/26/17 *********************
     O         OR    22
      *** End Add *** 09/26/17 *********************
     O         OR    24
      *** End Chg *** 01/21/16 *********************
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       @@AMT                  '  ,   , $0.  '
     O*
      *** Beg Add *** 01/21/16 *********************
      * CLT065
     O          E    23      CHECK       0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x500Y'
     O                       @@AMT                  '  ,   , $0.  '
      *** End Add *** 01/21/16 *********************
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       @@AMT1
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       @@AMT2
      ***
      *** beg chg *** 10/20/16 **************
     O          E   N62      CHECK       0  0
     O********* E            CHECK       0  0
      *** end chg *** 10/20/16 **************
      *  Courier 10, using 10 point, 10 pitch  "Bitmapped" font
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p10h12v0s0b3T'
      *
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
      *                                           CHECK NAME/ADDRESS
      *** beg chg *** 10/20/16 **************
     O********* E            CHECK       0  0
     O          E   N62      CHECK       0  0
      *** end chg *** 10/20/16 **************
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
      *
      *  UNDERLINE FOR FIRST SIGNATURE
      *
      *** beg chg *** 04/11/18 *************************************
     O********* E    86N75 78CHECK       0  0
     O          E    84N75 78CHECK       0  0
      *** end chg *** 04/11/18 *************************************
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *  Print underline for Second Signature in 2 situations:
      *     1) Second Signature is entered.
      *     2) Zero < $$$ Limit < Check Amount.
      *
     O          E    83N75   CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *********************************************************************
      *** Begin Change  *** 08/21/18 **************************************
     O**        E            CHECK       0  0
     O          E   N19      CHECK       0  0
      *** End   Change  *** 08/21/18 **************************************
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*
      *       E 00             CHECK
     O                       ESC
     O                                              '*p2020x550Y'
     O                       @@VOID
      **** Begin Add *** 08/21/18 *****************************************
      *
     O          E    19      CHECK       0  0
      *** Begin Change 03/06/19 *******************************************
      *                                                Font Change 12Pt
     O***                    ESC
     O***                                           '(8U'
     O***                    ESC
     O***                                           '(s0p10h12v0s0b3T'
     O***                    ESC
     O***                                           '*p1775x530Y'
     O***                    @@VOID
      *---------------------------------------------------------------------
      *   Change Font to COURIER 12 BOLD to VOID AFTER and Var Text.
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p10h12v0s3b3T'
     O                       ESC
     O                                              '*p1695x575Y'
      *---------------------------------------------------------------------
     O                       @@VOID23
      *** End   Change 03/06/19 *******************************************
     O          E    19      CHECK       0  0
     O                       ESC
     O                                              '*p1695x620Y'
     O                       ESC
     O                                              '&d1D'
     O                       BITXTONCK
      *** Begin Add    03/06/19 *******************************************
     O                       BITXTONCK2
      *** End   Add    03/06/19 *******************************************
     O                       ESC
     O                                              '&d@'
      *
      **** End   Add *** 08/21/18 *****************************************
      *********************************************************************
      *  Signatures and $$$ limit
      *
     O          E    85N75   CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*
      *       E 00     85N75   CHECK
     O                       ESC
     O                                              '*p1400x605Y'
     O*       E 00     85N75   CHECK
     O                       SIGMS1
     O                       BI$LMT                 ' ,   ,   , $0'
     O                       SIGMS2
      *
     O          E    87N75   CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD2
     O                       CH2
      *
     O          E    86N75   CHECK       0  0
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD1
     O                       CH1
      *
      *********************************************************************
      *  Voucher
     O          E    75      CHECK       0  0
      *  FOR BOLD FONT                                 Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
      *
     O          E    75      CHECK       0  0
      *  Print Voucher Account
     O                       ESC
     O                                              '*p1500x700Y'
     O                       @@ACCT
     O                       ESC
      *  Print 'VOUCHER'
     O                                              '*p1500x840Y'
     O                       VOUCHR
      *
     O          E    75      CHECK       0  0
      *  FOR NORMAL FONT                               Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      * *************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      * *************************************************************
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
