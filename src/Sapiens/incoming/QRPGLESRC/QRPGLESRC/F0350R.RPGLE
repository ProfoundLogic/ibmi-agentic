     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*** Beg Add ***** 11/22/16 **************************************
     H Bnddir('STBNDDIR')
     H*** End Add ***** 11/22/16 **************************************
     H*****************************************************************
     H/TITLE F0350R - Work with Provider Bills
     H*****************************************************************
     H*  CREATED: 04/01/98  C:56 Project: MC152                       *
     H*****************************************************************
    H** REVISIONS:
    H*                                                               *
     H*  03/19/24  C:079 Ticket: 99-115253                            *
     H*                  Account for state not installed (FTABLxx)    *
    H*                                                               *
     H*  06/09/20  C:295 Project : SY420                              *
     H*                  Mask SSN                                     *
     h*                                                               *
    H*  02/27/20 C:212 Project: MC435                                *
    H*                 Redo Provider Bill Subset as per CS standards *
    H*                                                               *
    H*  06/26/19 C:079 Project: 152MAP                               *
    H*                 Populate image doc ID in FAEOP                *
     h*                                                               *
     h*  05/22/19 C:212 Project: MC430                                *
     h*                 Use new CS program for Provider Bill detail   *
     h*                                                               *
     h*  05/03/19 C:079 Project: MC248                                *
     h*                 Use new CS program for Detail Charges Prompt  *
     h*                                                               *
     h*  04/10/19 C:062 Project: 152WAB                               *
     h*                 Apply CSI mods for S1 Medical.                *
     H*
     H*  11/15/17 C:079 Support: 025/50205
     H*                 Replace IN44 with errClmLck
     H*
     H*  10/11/17 C:079 Support: 025/50205
     H*                 Clear IN44 for Claim Lock Error
     H*
     H*  02/03/17 C:079 Support: 114/45374
     H*                 Read FAEOP without a record lock
     H*
     H*  01/25/17 C:079 Support: 114/45374
     H*                 Added error check for locked claim
     H*
     H*  11/22/16 C:309 Project: 25MUJ
     H*                 Added Check box for "Bill Import Error" Flag
     H*
     H*  07/29/16 C:212 Support: 42586/99
     H*                 Fix Sub# in DSPF to take in 5 chars
     H*
     H*  07/26/16 C:079 Support: 42586/99
     H*                 Change compare of MOD to be *zeros not *blanks
     H*
     H*  09/28/15 C:079 Project: 89MBM
     H*                 Add client 89 to PBills lite using flag
     H*
     H*  06/12/15 C:079 Project: 146MFY
     H*                 Remove cient 146 from Pbills lite
     H*                                                               *
     H*  06/11/15 C:079 Support: MC146 34589                          *
     H*                 On FEIN/Mod subset edit pass zeros for Mod    *
     H*
     H*  04/30/15 C:062 Support: 34027(048)
     H*                 Don't close files if RSTPGM = *On
     H*
     H*  03/03/15 C:079 Support: 25/32727
     H*                 Dont change the call mode when displaying options
     H*
     H*  08/06/14 C:079 Project: 146MFY
     H*                 Add client 146 to Pbills lite
     H*
     H*  03/25/14 C:296 Project: MC376                                *
     H*                 Populate status as 'E' when it is a accepted  *
     H*                 record (IA).                                  *
     H*
     H*  04/29/14 C:062 Support: WC112 85494                          *
     H*                Don't allow non-numerics from entering in EOB  *
     H*                position to field.
     H*
     H*  02/03/14 C:291 Redmine: 2382                                 *
     H*                 Fixed "Position to" fields width.             *
     H*                                                               *
     H*  03/20/13 C:284 Project: 46WHE
    H*                 Do not allow add if client id = 46. This client
    H*                 is having provider bill lite.
     H*
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Add client 114 for the edit in ERRCK1         *
     H*                                                               *
     H*  01/09/13 C:280 RedMine: #1963
     H*                 Fix bug with delete override command
     H*
     H*  01/04/13 C:277 Project: 114MER
     H*                 Temprarly removing project 114MBB changes
     H*
     H*  11/13/12 C:287 Project: MC367
     H*                 Remove options/functions for GS access to Bill Entry
    H*
    H*  08/10/12 C:280 Project: 144MCF
    H*                 Added print image doc id on eomb option
     H*
     H*  06/27/12 C:33 Project: SY266
     H*                Add override scope
    H*
    H*  06/15/12 C:280 Project: 114MER
    H*                 Added error screen to stop code review
    H*
    H*  12/14/11 C:251 Project: 114MBB
    H*                 Added error screen to stop modification of bill when payment
    H*                 limit is reached
    H*
    H*  07/28/11 C:257 Support: 77403
    H*                 Option '21' should not be visible when called from W0070R50
    H*
    H*  05/03/11 C:257 Support: 75607
    H*                 SA008R should be checeked in ERRCK1.
    H*
    H*  12/18/10 C:227 Project: 146MCV
    H*                 Added edit not to allow processing on EOB's sent
    H*                 to Procura
    H*
    H*  10/14/08 C:62  Project: 25MPE
    H*                 Add EAIDOC subset processing
    H*
    H*  12/08/07 C:33  Project: 114WMF  OP144/61405
    H*                Change WMXRL to be User Controlled.  Only open the
    H*                 file if WCMS is installed.
    H*
    H*  10/29/07 C:47  PROJECT MC309
    H*                 Display reconsiderations in red.
    H*
    H*  08/08/07 C:65  PROJECT 114WMF/Support
    H*                 User UPDOFA to clear options within program.
    H*
    H*  05/25/07 C:191 PROJECT: WC1515
    H*                 Check claims authority
    H*
    H*  11/03/06 C:079 SUPPORT: 55448
    H*                 ONLY CALL F0350C1 ONCE
    H*
    H* 10/06/06 C:53  PROJECT 114WMF    114WIH 04/12/05
    H*                 Block User Access to Claims
    H*
    H*  09/27/06 C:181 PROJECT: 10MJE
    H*                 IF FL MEDICAL EDI BILL SUBMISSION HAS BEEN MADE
    H*                 AND BILL HAS NOT BEEN WITHDRAWN, DISPLAY ERROR
    H*                 MESSAGE TO WITHDRAW BILL BEFORE DELETE WILL BE
    H*                 ALLOWED.
    H*
    H*  04/19/06 C:163 PROJECT 114WMF                                *
    H*                 Back to Base - EOMB Processing (114MEX,114MFC)*
    H*                                                               *
    H*  06/06/06 C:79  PROJECT: 139WCP
    H*                 ADD CLIENT 139 TO PROVIDER BILLS LITE
    H*
    H*  02/17/06 C:79  SUPPORT: 49396
    H*                 SINCE KY NO LONGER REQUIRES EDI DONT CHECK THE LOG
    H*                 RECORD WHEN CHANGING THE BILL
    H*
    H*  11/29/05 C:79  PROJECT: 141MAI
    H*                 ADD CLIENT 141 TO PROVIDER BILLS LITE
    H*
    H*  11/16/05 C:79  PROJECT: 136WAA
    H*                 ADD LOGIC FOR CLIENTS WITH PROVIDER BILLS LITE
    H*
    H*  08/25/05 C:79  PROJECT: 10MGS
    H*                 ADD IMAGE DOC ID VIEW
    H*
    H*  08/08/05 C:79  PROJECT: 129MJR
    H*                 ADD CALL TO VIEW DOCUMENT.
    H*
    H*  04/15/04 C:79  PROJECT: FL046
    H*                 FLORIDA: ALLOW EDI CORRECTIONS WHEN CALLING F0395R01
    H*                 UNDER CERTAIN CONDITIONS
    H*
    H*  08/25/03 C:62  PROJECT: MC048 38682
    H*                 CORRECT CONTROL NUMBER VIEW POSITIONING
    H*
    H*  05/16/03 C:79  PROJECT: FS249
    H*                 ADD OPTION 20=PENDED ITEMS
    H*                 FIX PROV SRCH CALL NOT TO RETURN BAD DATA
    H*
    H*  10/29/02 C:62  PROJECT: MC253
    H*                 ADD MULTI GROUP RESTRICTIONS LOGIC.
    H*
    H*  06/14/02 C:062 PROJECT: 25MNB
    H*                 KEEP FROM CHANGEING OR DELETEING IF SENT TO ROCKPORT
    H*
    H*  05/30/02 C:062 PROJECT: FS245
    H*                 ADD SECURITY LOGIC.
    H*
    H*  05/23/02 C:062 PROJECT: 114MDC
    H*                 ADD PROCESSING FOR CODEREVIEW PEND ERROR SCREEN
    H*
    H*  02/11/02 C:062 PROJECT: TX017
    H*                 -VALID EDI FLAG IS 'Y' AND 'R'.
    H*
    H*  01/02/02 C:062 PROJECT: MC095 30483
    H*                 REMOVE FIELD PROTECTION OF SSFEIN/SSMOD.
    H*
    H*  12/10/01 C:062 PROJECT: FS233
    H*                 DEFAULT SERVICE DATE TO TODAYS DATE IF
    H*                 POSITION TO IS BLANK.
    H*
    H*  11/19/01 C:062 PROJECT: MC034 29859
    H*                 CORRECT THE POPULATION OF OUTQ FOR OPTION 12.
    H*
    H*  09/07/01 C:062 PROJECT: FS233
    H*                 CHANGE NAME LOGICALS TO INCLUDE FROM DATE.
    H*
    H*  06/01/01 C:062 PROJECT: MCMNT
    H*                 INITIALIZE CONTROL NUMBER KEY FIELD.
    H*
    H*  06/01/01 C:062 PROJECT: FS227
    H*                 ADD LOGIC FOR ADDITION OF BILL TYPE SUBSET
    H*
    H*  05/24/01 C:062 PROJECT: FSMNT # 192  BASE ONLY
    H*                 ADD PARM TO CALL OF F9951R, TO LET IT KNOW
    H*                 THAT IT IS NOT BEING CALLED FROM THE MENU.
    H*
    H*  12/29/00 C:117 PROJECT: FS209
    H*                 CHANGE OPT16=HISTORY TO USE NEW PGMS
    H*
    H*  11/10/00 C:062 PROJECT: MC051 24990
    H*                 MAKE SURE IF F12 IS USED WITH OPT4 THAT IT
    H*                 DOESN'T CLOSE WDPALA.
    H*
    H*  10/13/00 C:117 PROJECT: MC217
    H*                 ADD OPTION 22=VIEW PROV INFO
    H*
    H*  09/27/00 C:117 PROJECT: FS214
    H*                 ADDING 2 MORE INFOS TO SCREEN:
    H*                 INFO#3 - CONTROL NUMBER
    H*                 INFO#4 - PROVIDER'S NAME
    H*
    H*  09/25/00 C:062 PROJECT: MC010 24451
    H*                 CONDITION WDPAP.
    H*
    H*  08/15/00 C:062 PROJECT: FS213
    H*                 ENABLE OPTIONS '5', '7', '10', '11', '13', '15'
    H*                 , '16' AND '82' IN SEARCH MODE.
    H*
    H*  07/13/00 C:062 PROJECT: MC025 23514
    H*                 CHANGE WHAT SEC LEVELS/TYPES ARE TO BE USED.
    H*
    H*  07/11/00 C:079 PROJECT: MCMNT
    H*                 Add two more conditions for opening WDPALA
    H*
    H*  06/07/00 C:117 PROJECT: MCMNT
    H*                 Changed type of call to F0355R; needs to be
    H*                 display only.
    H*
    H*  05/18/00 C:079 PROJECT: MC044 22934
    H*                 1. Add security logic for 4=Delete.
    H*
    H*  04/03/00 C:062 PROJECT: MC048 22221
    H*                 -MAKE SURE F12 AND F17 ARE AVAILABLE IN SEARCH.
    H*                 -PASS BACK EOB# IF IN SEARCH MODE.
    H*
    H*  03/08/00 C:117 PROJECT: FSMNT
    H*                 ADDING A CNTL# VIEW TO MAIN WORK WITH
    H*                 HAD TO ADD API'S
    H*
    H*  03/01/00 C:53  Support: KY010
    H*                 1. CORRECT THE ACCIDENT DATE BEING PASSED TO
    H*                    F0875R
    H*
    H*  02/23/00 C:79  Support: MC044/#21138
    H*                 1. Added call to F0350C02 to place a lock on
    H*                    FDAILYA at program start.
    H*                 2. Added call to F0350C03 to release the lock
    H*                    on FDAILYA at program end.
    H*
    H*  01/25/00 C:15  Project: KY010
    H*                 Added code to process opt 18 and 19.
    H*
    H*  11/19/99 C:56  Support: MC044 #20349
    H*                 Correct the FAEOP relative record number sent
    H*                 to ACDPGM = F0350R01 when not in View 1.
    H*
    H*  10/27/99 C:56  Support: MC010  #20357
    H*                 Correct override to FMSVL1 and FMSVL2
    H*
    H*  09/10/99 C:56  Project: KY010
    H*                 Add changes to opt2=Changd to accomodate tape
    H*                 sumbission/EDI processing.
    H*
    H*  07/29/99 C:31  Project: MC152
    H*                 V4: Bill Entry re-write
    H*                 1. Add option 3 = Copy
    H*
    H*  07/19/99 C:62  Project: MC208
    H*                 CHANGED HEADING OF TYPE TO TYP AND ADDED ADJ
    H*                 STATE HEADING.
    H*
    H*  06/24/99 C:62  Project: MC202
    H*                 ADDING OPTION '30' = LETTERS.
    H*
    H*  06/17/99 C:53  Project: MC145
    H*                 QUALIFY WCMS OPTIONS ON NON-PDS WCMS FLAG
    H*
    H*  06/02/99 C:56  Project: MC152
    H*                 Set the initial view base on system control
    H*                 flag FBAL05 pos. 2.
    H*
    H*  05/12/99 C:56  Project: MC152
    H*                 For option7=Work with Detail, add overrides
    H*                 to FMPRP, FMSVL1 and FMSVP.
    H*
    H*  05/07/99 C:56  Project: MC152
    H*                 Condition use of Opt=2 for reconsideration
    H*
    H*  04/27/99 C:62  Project: MC025 17171
    H*                 REPLACE FEIN/MOD LOGICALS WITH FEIN LOGICALS.
    H*
    H*  04/13/99 C:62  Project: MC025 17171
    H*                 IF SUBSET ON MOD AND MOD IS NOT EQUAL TO ZERO
    H*                 PLACE THAT VALUE IN SXMOD.
    H*
    H*  04/12/99 C:31  Project: MC195
    H*                 F0350R:Chg Prov nm to FEIN
    H*                 1. SCR01 - PNAM replaced with FEIN/MOD
    H*                 2. SCR03 - PNAM replaced with FEIN/MOD
    H*                 3. SCR04 - PNAM replaced with FEIN/MOD
    H*
    H*  04/08/99 C:31  Project: MC193
    H*                 Work with Provider Bills: new options
    H*                 1. Add option 16 = History support
    H*                 2. Add option 81 = Claim info support
    H*                 3. Add option 82 = Case info support
    H*
    H*  02/23/99 C:62  Project: MC152
    H*                 PROTECT SUBSET OF CASE IF PASSED IN.
    H*
    H*  01/11/99 C:31  Project: MC152
    H*                 V4: Bill Entry re-write
    H*                 1. Complete option 21 support
    H*                 2. Complete W0070R50 interface
    H*                 3. Add option 6
    H*
    H*  12/27/98 C:62 PROJECT 25MKK
    H*                ADD ENTRY PARMS FOR EOB# AND PRE-CERT#, IF
    H*                VALUES ARE PASSED IN THEN SUBSET ON EOB# AND/OR
    H*                CERT#.
    H*
    H*****************************************************************
    Hdebug(*yes) option(*srcstmt:*nodebugio)
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff0350f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
    F*
     Ffaeop     if   e             disk
     F                                     rename(peo01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
    F*
VW#1 Ffaeolh    if   e           k disk    usropn
     F                                     infds(dbfds)
     F                                     rename(peo01:dbrcdf)
    F*
VW#2 Ffaeoli    if   e           k disk    usropn
     F                                     infds(dbfds2)
     F                                     rename(peo01:dbrcd2)
    F*
VW#3 Ffaeolj    if   e           k disk    usropn
     F                                     infds(dbfds3)
     F                                     rename(peo01:dbrcd3)
    F*
VW#4 Ffaeolk    if   e           k disk    usropn
     F                                     infds(dbfds4)
     F                                     rename(peo01:dbrcd4)
    F*
     Ffaeoln    if   e           k disk
     F                                     infds(dbfdsc)
     F                                     rename(peo01:dbrcdc)
    F*
     Ffaeolo    if   e           k disk
     F                                     infds(dbfdsd)
     F                                     rename(peo01:dbrcdd)
    F*
     Ffaeolp    if   e           k disk
     F                                     infds(dbfdse)
     F                                     rename(peo01:dbrcde)
    F*
     Ffaeolq    if   e           k disk
     F                                     infds(dbfdsg)
     F                                     rename(peo01:dbrcdg)
    F*
     Ffaeol25   if   e           k disk
     F                                     infds(dbfdsh)
     F                                     rename(peo01:dbrcdh)
    F*
     Ffaeol27   if   e           k disk
     F                                     infds(dbfdsi)
     F                                     rename(peo01:dbrcdi)
    F*
     Ffaeol28   if   e           k disk
     F                                     infds(dbfdsk)
     F                                     rename(peo01:dbrcdk)
    F*
     Ffaeol30   if   e           k disk
     F                                     infds(dbfdsl)
     F                                     rename(peo01:dbrcdl)
    F*
     Ffaeolw    if   e           k disk
     F                                     infds(dbfdsm)
     F                                     rename(peo01:dbrcdm)
    F*
     Ffaeolx    if   e           k disk
     F                                     infds(dbfdsn)
     F                                     rename(peo01:dbrcdn)
    F*
     Ffaeoly    if   e           k disk
     F                                     infds(dbfdso)
     F                                     rename(peo01:dbrcdo)
    F*
     Ffaeolz    if   e           k disk
     F                                     infds(dbfdsp)
     F                                     rename(peo01:dbrcdp)
    F*
     Ffaeol10   if   e           k disk
     F                                     infds(dbfdsq)
     F                                     rename(peo01:dbrcdq)
    F*
     Ffaeol11   if   e           k disk
     F                                     infds(dbfdsr)
     F                                     rename(peo01:dbrcdr)
    F*
     Ffaeol12   if   e           k disk
     F                                     infds(dbfdss)
     F                                     rename(peo01:dbrcds)
    F*
     Ffaeol13   if   e           k disk
     F                                     infds(dbfdst)
     F                                     rename(peo01:dbrcdt)
    F*
     Ffaeol14   if   e           k disk
     F                                     infds(dbfdsu)
     F                                     rename(peo01:dbrcdu)
    F*
     Ffaeol15   if   e           k disk
     F                                     infds(dbfdsv)
     F                                     rename(peo01:dbrcdv)
    F*
     Ffaeol16   if   e           k disk
     F                                     infds(dbfdsw)
     F                                     rename(peo01:dbrcdw)
    F*
     Ffaeol17   if   e           k disk
     F                                     infds(dbfdsx)
     F                                     rename(peo01:dbrcdx)
    F*
     Ffaeol18   if   e           k disk
     F                                     infds(dbfdsy)
     F                                     rename(peo01:dbrcdy)
    F*
     Ffaeol19   if   e           k disk
     F                                     infds(dbfdsz)
     F                                     rename(peo01:dbrcdz)
    F*
     Ffaeol20   if   e           k disk
     F                                     infds(dbfdt1)
     F                                     rename(peo01:dbrct1)
    F*
     Ffaeol21   if   e           k disk
     F                                     infds(dbfdt2)
     F                                     rename(peo01:dbrct2)
    F*
     Ffaeol22   if   e           k disk
     F                                     infds(dbfdt3)
     F                                     rename(peo01:dbrct3)
    F*
     Ffaeol23   if   e           k disk
     F                                     infds(dbfdt4)
     F                                     rename(peo01:dbrct4)
    F*
     Ffaeol29   if   e           k disk
     F                                     infds(dbfdt5)
     F                                     rename(peo01:dbrc05)
    F*
     Ffaeol26   if   e           k disk
     F                                     infds(dbfdt6)
     F                                     rename(peo01:dbrc06)
    F*
     Ffaeol37   if   e           k disk
     F                                     infds(dbfd07)
     F                                     rename(peo01:dbrc07)
    F*
     Ffaeol38   if   e           k disk
     F                                     infds(dbfd08)
     F                                     rename(peo01:dbrc08)
    F*
     Ffaeol39   if   e           k disk
     F                                     infds(dbfd09)
     F                                     rename(peo01:dbrc09)
    F*
     Ffaeol40   if   e           k disk
     F                                     infds(dbfd10)
     F                                     rename(peo01:dbrc10)
    F*
     Ffaeol41   if   e           k disk
     F                                     infds(dbfd11)
     F                                     rename(peo01:dbrc11)
    F*
     Ffaeol42   if   e           k disk
     F                                     infds(dbfd12)
     F                                     rename(peo01:dbrc12)
    F*
     Ffaeol43   if   e           k disk
     F                                     infds(dbfd13)
     F                                     rename(peo01:dbrc13)
    F*
     Ffaeol44   if   e           k disk
     F                                     infds(dbfd14)
     F                                     rename(peo01:dbrc14)
    F*
VW#5 Ffaeol32   if   e           k disk    usropn
     F                                     infds(dbfds5)
     F                                     rename(peo01:dbrcd5)
    F*
VW#1 Ffaeol46   if   e           k disk
     F                                     infds(dbfd15)
     F                                     rename(peo01:dbrc15)
    F*
VW#2 Ffaeol47   if   e           k disk
     F                                     infds(dbfd16)
     F                                     rename(peo01:dbrc16)
    F*
VW#3 Ffaeol48   if   e           k disk
     F                                     infds(dbfd17)
     F                                     rename(peo01:dbrc17)
    F*
VW#4 Ffaeol49   if   e           k disk
     F                                     infds(dbfd18)
     F                                     rename(peo01:dbrc18)
    F*
VW#5 Ffaeol50   if   e           k disk
     F                                     infds(dbfd19)
     F                                     rename(peo01:dbrc19)
    F*
VW#1 Ffaeol56   if   e           k disk
     F                                     infds(dbfd20)
     F                                     rename(peo01:dbrc20)
    F*
VW#2 Ffaeol57   if   e           k disk
     F                                     infds(dbfd21)
     F                                     rename(peo01:dbrc21)
    F*
VW#3 Ffaeol58   if   e           k disk
     F                                     infds(dbfd22)
     F                                     rename(peo01:dbrc22)
    F*
VW#4 Ffaeol59   if   e           k disk
     F                                     infds(dbfd23)
     F                                     rename(peo01:dbrc23)
    F*
VW#5 Ffaeol60   if   e           k disk
     F                                     infds(dbfd24)
     F                                     rename(peo01:dbrc24)
    F*
VW#1 Ffaeol61   if   e           k disk
     F                                     infds(dbfd25)
     F                                     rename(peo01:dbrc25)
    F*
VW#2 Ffaeol62   if   e           k disk
     F                                     infds(dbfd26)
     F                                     rename(peo01:dbrc26)
    F*
VW#3 Ffaeol63   if   e           k disk
     F                                     infds(dbfd27)
     F                                     rename(peo01:dbrc27)
    F*
VW#4 Ffaeol64   if   e           k disk
     F                                     infds(dbfd28)
     F                                     rename(peo01:dbrc28)
    F*
VW#5 Ffaeol65   if   e           k disk
     F                                     infds(dbfd29)
     F                                     rename(peo01:dbrc29)
    F*
VW#6 Ffaeol75   if   e           k disk    usropn
     F                                     infds(dbfds6)
     F                                     rename(peo01:dbrc30)
    F*
    F*  Claim Master File
    F*
     Ffmcml1    if   e           k disk
     F                                     infds(dbfcm1)
    F*
    F*  EOMB Status Change Transaction File
    F*
     Ffaesl     if   e           k disk
     F                                     infds(dbfda)
    Ffaeal1    if   e           k disk
    F*
     Ffaeal42   if   e           k disk             rename(pea01:pea02)
    F*
     Fsmupl     if   e           k disk
    F*
     Fsmupl1    if   e           k disk    rename(pup01:pup02)
    F*
     Ffmsml     if   e           k disk
    F*
     Ffmfnl1    if   e           k disk
    F*
     Ffmfnl     if   e           k disk
    F*
     F                                     rename(pfn01:pfn02)
     Fimdtl     if   e           k disk
    F*
     Fimdol1    if   e           k disk
    F*
     FWMXRL     if   e           k disk    usropn
    F*** Beg Add *** 01/25/17 ****************************************
     Ffxlop     if   e           k disk
    F*** End Chg *** 01/25/17 ****************************************
    F*** Beg Add *** 06/26/19 ****************************************
     Ffau1l2    if   e           k disk
     Foptimptl2 if   e           k disk
    F*** End Chg *** 06/26/19 ****************************************
    D*
     D hd              s             78    dim(24) ctdata perrcd(1)
     D of              s              1    dim(36) ctdata perrcd(1)
     D ofd             s             35    dim(36) alt(of)
     D grp             s              3    dim(999)
    D*** BEG CHG *** 05/12/99 ****************************************
     D***  Begin Chg  ***  06/27/12  **************************************
     D*qca******       s             56    dim(14) ctdata perrcd(1)
     D qca             s             60    dim(14) ctdata perrcd(1)
     D***  End   Chg  ***  06/27/12  **************************************
    D******************  QCA     1  12 56               CMD EXEC/QCMDEXC
    D*** END CHG *** 05/12/99 ****************************************
     D data            s             74    dim(6)
     D dta1            s              1    dim(74)
     D dta2            s              1    dim(74)
     D dta3            s              1    dim(74)
     D dta4            s              1    dim(74)
     D dta5            s              1    dim(74)
     D dta6            s              1    dim(74)
     D inf1            s              1    dim(29)
     D inf2            s              1    dim(29)
     D inf3            s              1    dim(29)
     D inf4            s              1    dim(29)
    D*
     D dbfda           ds
     D dbrrna                397    400b 0
     D dbfdsc          ds
     D dbrrnc                397    400b 0
     D dbfdsd          ds
     D dbrrnd                397    400b 0
     D dbfdse          ds
     D dbrrne                397    400b 0
     D dbfdsg          ds
     D dbrrng                397    400b 0
     D dbfdsh          ds
     D dbrrnh                397    400b 0
     D dbfdsi          ds
     D dbrrni                397    400b 0
     D dbfdsj          ds
     D dbrrnj                397    400b 0
     D dbfdsk          ds
     D dbrrnk                397    400b 0
     D dbfdsl          ds
     D dbrrnl                397    400b 0
     D dbfdsm          ds
     D dbrrnm                397    400b 0
     D dbfdsn          ds
     D dbrrnn                397    400b 0
     D dbfdso          ds
     D dbrrno                397    400b 0
     D dbfdsp          ds
     D dbrrnp                397    400b 0
     D dbfdsq          ds
     D dbrrnq                397    400b 0
     D dbfdsr          ds
     D dbrrnr                397    400b 0
     D dbfdss          ds
     D dbrrns                397    400b 0
     D dbfdst          ds
     D dbrrnt                397    400b 0
     D dbfdsu          ds
     D dbrrnu                397    400b 0
     D dbfdsv          ds
     D dbrrnv                397    400b 0
     D dbfdsw          ds
     D dbrrnw                397    400b 0
     D dbfdsx          ds
     D dbrrnx                397    400b 0
     D dbfdsy          ds
     D dbrrny                397    400b 0
     D dbfdsz          ds
     D dbrrnz                397    400b 0
     D dbfdt1          ds
     D dbrr01                397    400b 0
     D dbfdt2          ds
     D dbrr02                397    400b 0
     D dbfdt3          ds
     D dbrr03                397    400b 0
     D dbfdt4          ds
     D dbrr04                397    400b 0
     D dbfcm1          ds
     D dbrcm1                397    400b 0
     D dbfdt5          ds
     D dbrr05                397    400b 0
     D dbfdt6          ds
     D dbrr06                397    400b 0
     D dbfd07          ds
     D dbrr07                397    400b 0
     D dbfd08          ds
     D dbrr08                397    400b 0
     D dbfd09          ds
     D dbrr09                397    400b 0
     D dbfd10          ds
     D dbrr10                397    400b 0
     D dbfd11          ds
     D dbrr11                397    400b 0
     D dbfd12          ds
     D dbrr12                397    400b 0
     D dbfd13          ds
     D dbrr13                397    400b 0
     D dbfd14          ds
     D dbrr14                397    400b 0
     D dbfd15          ds
     D dbrr15                397    400b 0
     D dbfd16          ds
     D dbrr16                397    400b 0
     D dbfd17          ds
     D dbrr17                397    400b 0
     D dbfd18          ds
     D dbrr18                397    400b 0
     D dbfd19          ds
     D dbrr19                397    400b 0
     D dbfd20          ds
     D dbrr20                397    400b 0
     D dbfd21          ds
     D dbrr21                397    400b 0
     D dbfd22          ds
     D dbrr22                397    400b 0
     D dbfd23          ds
     D dbrr23                397    400b 0
     D dbfd24          ds
     D dbrr24                397    400b 0
     D dbfd25          ds
     D dbrr25                397    400b 0
     D dbfd26          ds
     D dbrr26                397    400b 0
     D dbfd27          ds
     D dbrr27                397    400b 0
     D dbfd28          ds
     D dbrr28                397    400b 0
     D dbfd29          ds
     D dbrr29                397    400b 0
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D                 ds
     D***  Begin Chg  ***  06/27/12  **************************************
     D*qqcmd****                     56
     D qqcmd                         60
     D***  End   Chg  ***  06/27/12  **************************************
     D  qqst                          2    overlay(qqcmd:38)
    D*
     D                 ds
     D***  Begin Chg  ***  06/27/12  **************************************
     D*qqcmd2****                    56
     D qqcmd2                        60
     D***  End   Chg  ***  06/27/12  **************************************
     D  qqst2                         2    overlay(qqcmd2:40)
    D*
     D                 ds
     D x1dt                          10
     D  x1dtm                         2    overlay(x1dt)
     D  x1dtd                         2    overlay(x1dt:4)
     D  x1dty                         4    overlay(x1dt:7)
    D*
     D                 ds
     D  x2dt                   1      8
     D  x2dtm                  1      2
     D  x2dtd                  3      4
     D  x2dty                  5      8
     D qryslt          ds          5000
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
    D*
     D optnm           c                   const('Provider Bills')
     D optnm2          c                   const('Standalone add-a-remark')
     D slmsgi          c                   const('SYS0001')
     D slmsgf          c                   const('FSMESG')
     D acdpgm          c                   const('F0350R01')
     D comapi          c                   const('FA007R')
     D uspapi          c                   const('F0006R')
     D qrypgm          c                   const('F0350C1')
     D infomx          c                   const(4)
     D viewmx          c                   const(6)
     D sf1pag          c                   const(8)
    D*
    D*================================================================
    D*  Program specific constants
    D*================================================================
    D*
     D heada           c                   const('Dates of Service')
     D clropt          c                   const('                    -
     D                                            NNN')
     D pciopt          c                   const('1515=Pre-Cert Info  -
     D                                            YYN')
     D clmopt          c                   const('8181=Claim info     -
     D                                            YYN')
     D ediopt          c                   const('1818=EDI Log Info   -
     D                                            YYN')
     D ssqopt          c                   const('1919=Send Subsq 837 -
     D                                            YNN')
     D vckopt          c                   const('2121=Check info     -
     D                                            YYN')
     D delete          c                   const('Deleted')
     D co#             c                   const('(EOCO# *EQ')
     D co#2            c                   const(') *AND (')
     D sub#            c                   const('(EOSUB# *EQ')
     D fnd             c                   const('*AND (EOFND *EQ -
     D                                     %VALUES(')
     D ovrdb1          c                   const('OVRDBF FILE(')
     D ovrdb2          c                   const(') SHARE(*YES)')
     D kkdoct          s                   like(dtdoct)
     D kkdcid          s                   like(dodcid)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
    D*
     D ##acdt          s              8  0
     D ##acis          s              1
     D ##ar            s              1
     D ##arin          s              1
     D ##ast           s                   like(eoast)
     D ##blst          s              1
     D ##bscl          s              3  0
     D ##bsct          s              1
     D ##btyp          s                   like(eobtyp)
     D ##c#cl          s              3  0
     D ##c#ct          s              1
     D ##call          s              1
     D ##case          s              7  0
     D ##cert          s             15
     D ##ckdl          s              3  0
     D ##ckdt          s              1
     D ##cldl          s              3  0
     D ##cldt          s              1
     D ##cltl          s              3  0
     D ##cltt          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##cont          s              1
     D ##cpaa          s              5
     D ##cpad          s              5
     D ##cs#l          s              3  0
     D ##cs#t          s              1
     D ##dccl          s              3  0
     D ##dcct          s              1
     D ##dcdl          s              3  0
     D ##dcdt          s              1
     D ##depg          s             10
     D ##desc          s             40
     D ##dnsl          s              3  0
     D ##dnst          s              1
     D ##edi           s              1
     D ##eob#          s                   like(eoeob#)
     D ##eobd          s              3  0
     D ##eobi          s              1
     D ##err           s              1
     D ##ext           s              3
     D ##fein          s              9
     D ##fimr          s              5
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##frn           s              5
     D ##frst          s             15
     D ##fsch          s              1
     D ##fyr           s              3  0
     D ##kc            s              1
     D ##key           s             40
     D ##kl            s              1
     D ##last          s             56
     D ##lseq          s              5  0
     D ##lvl           s              3  0
     D ##mcif          s              1
     D ##mciv          s              1
     D ##med           s              1
     D ##meus          s              1  0
     D ##mgid          s              1
     D ##mod           s              3
     D ##mode          s              1
     D ##mss           s              1
     D ##name          s             40
     D ##nerc          s              5
     D ##npwc          s              1
     D ##ntwk          s              1
     D ##nwpa          s              5
     D ##nwst          s              1
     D ##ocst          s              1
     D ##outq          s             10
     D ##outs          s              1
     D ##pay0          s              1
     D ##pbiv          s              1
     D ##pgm2          s             10
     D ##plcl          s              3  0
     D ##plct          s              1
     D ##prvb          s              1
     D ##pvmc          s              1  0
     D ##pvpr          s              1  0
     D ##que           s                   like(##outq)
     D ##rcn           s              3  0
     D ##reas          s             60
     D ##rhab          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##rrn3          s              9  0
     D ##rtcd          s              1
     D ##sdcl          s              3  0
     D ##sdct          s              1
     D ##sdts          s              1
     D ##sort          s              2
     D ##ss#           s              9
     D ##stcd          s              3
     D ##sts           s              2
     D ##sub#          s              5  0
     D ##sys           s                   like(eosys)
     D ##tbid          s              4
     D ##term          s              1
     D ##timr          s              5
     D ##trn           s              5
     D ##tset          s              3
     D ##type          s              1
     D ##ur            s              1
     D ##user          s             10
     D ##usnm          s             50
     D ##view          s              1  0
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     D ##wcup          s              1
     D ##1n4           s              1
     D ##1prv          s              1
     D #1case          s              7
     D #1co#           s              3
     D #1eob#          s              9
     D #1err           s              1
     D #1ext           s              3  0
     D #1fein          s              9  0
     D #1fnd           s              3
     D #1fyr           s              3
     D #1key           s             10
     D #1l1            s             78
     D #1l2            s             78
     D #1mod           s              3  0
     D #1mode          s              1
     D #1name          s             40
     D #1pgm           s              8
     D #1rrn           s              9
     D #1rtcd          s              1
     D #1sub#          s              5
     D #1wcf#          s              3
     D #1wcin          s              1
     D #1wdck          s              1
     D #2case          s              7  0
     D #2err           s              1
     D #2fnd           s              3  0
     D #2fyr           s              3  0
     D #2name          s             40
     D #2rrn           s              9  0
     D #2sub#          s              5  0
     D @@call          s              1
     D @@case          s              7  0
     D @@cert          s             15
     D @@eob#          s              9  0
     D @@fnd           s              3  0
     D @@fyr           s              3  0
     D @@rcn           s              3  0
     D @@rrn2          s              9  0
     D @@sfdt          s              8
     D @@sfei          s              9
     D @@smod          s              3
     D @@stdt          s              8
     D @@sub#          s              5  0
     D @3blst          s              1
     D @3kc            s              1
     D @3kl            s              1
     D @3mode          s              1
     D @3pgm           s              8
     D @3reas          s             60
     D @3rrn           s              9  0
     D @3rrn2          s              9  0
     D @3rrn3          s              9  0
     D @3stcd          s              3
     D alchrg          s             10
     D aleob#          s              9
     D alfein          s              9
     D almod           s              3
     D alpaid          s             10
     D hlblst          s              1
     D hlbsds          s             30
     D hlcase          s                   like(eocase)
     D hldate          s              8
     D hlfein          s                   like(eofein)
     D hlfnd           s                   like(eofnd)
     D hlfnnm          s             30
     D hlfrdt          s                   like(eofrdt)
     D hlfyr           s                   like(eofyr)
     D hlinf1          s             29
     D hlinf2          s             29
     D hlinf3          s             29
     D hlinf4          s             29
     D hliusr          s                   like(eouser)
     D hlmod           s                   like(eomod)
     D hlpnam          s                   like(eopnam)
     D hlsdoi          s                   like(eoacdt)
     D hlsmnm          s             30
     D hlsub#          s                   like(eosub#)
     D hltodt          s                   like(eofrdt)
     D hlusnm          s             40
     D kkacdt          s                   like(eoacdt)
     D kkcase          s                   like(cmcase)
     D kkcert          s             15
     D kkcntl          s                   like(eocntl)
     D kkidoc          s                   like(eoidoc)
     D kkeob#          s                   like(eoeob#)
     D kkext           s              3  0
     D kkfein          s                   like(eofein)
     D kkfnd           s                   like(cmfnd)
     D kkfrdt          s                   like(eofrdt)
     D kkfsn           s                   like(eofsn)
     D kkfyr           s                   like(cmfyr)
     D kkidt           s                   like(esidt)
     D kkitim          s                   like(esitim)
     D kkkey           s             10
     D kklsn           s                   like(eolsn)
     D kkmod           s                   like(eomod)
     D kkpnam          s                   like(eopnam)
     D kkrcn           s                   like(eorcn)
     D kkss#           s                   like(eoss#)
     D kksub#          s                   like(cmsub#)
     D kktbid          s              4
     D kkusr           s                   like(upusr)
     D kkstaf          s                   like(upstaf)
     D length          s             15  5
     D qqacdt          s              7  0
     D qqactf          s              1
     D qqcase          s              7
     D qqco#           s              3
     D qqeob#          s              9
     D qqflag          s              1
     D qqfnd           s              3
     D qqfyr           s              3
     D qqmask          s             10
     D qqmod           s              3
     D qqmode          s              1
     D qqnum           s             80
     D qqrcn           s              3
     D qqrtcd          s              1
     D qqsub#          s              5
     D ssbsds          s             30
     D sspdnm          s             20
     D stop            s              1
     D vldopt          s              1
     D x               s              2  0
     D xxcntl          s                   like(eocntl)
     D xxmod           s              3  0
     D x1fein          s              9
     D x1mod           s              3
     D y               s              2  0
     D zzfein          s              9
     D zzmod           s              3
     D #2emp#          s                   like(upemp#)
     D $$fnd           s                   like(upfnd)
     D $$sub#          s                   like(upsub#)
     D svcall          s                   like(@@call)
     D svin09          s                   like(*in09)
     D ##mrem          s                   like(upmrem)
     D ##mrgr          s                   like(upmrgr)
     D ##mrsu          s                   like(upmrsu)
     D xxovrd          s             50
     D cmdstr          s             50
     D z               s              3  0
     D xaco#           s              3
     D xasub#          s              5
     D grperr          s              1
     D first           s              1
     D ##prom          s              1
     D ##preo          s              1
     D ##parn          s              1
     D ##os            s              1
     D ##mgri          s              1
     D ##mcrv          s              1
     D ##freq          s              1
     D ##crac          s              1
     D ##crvw          s              1
     D ##ccod          s              1
     D ##auth          s              1
     D ##arls          s              1
     D ##kcif          s              1
     D ##curs          s              1
     D ##pcib          s              1
     D ##clt#          s              3
     D*** Beg Add *** 09/28/15 ********************************************
     D ##ucpf          s              1
     D ##dcf           s              1
     D ##autb          s              1
     D ##deif          s              1
     D ##aure          s              1
     D ##cred          s              1
     D ##idrt          s              1
     D ##cprv          s              1
     D ##eft           s              1
     D ##govr          s              1
     D ##ncdc          s              1
     D ##splt          s              1
     D ##lst           s              1
     D ##pblt          s              1
     D ##inze          s              1
     D*** End Add *** 09/28/15 ********************************************
     D WMXRLopen       s              1n   inz(*off)
     D*** Beg Dlt *** 01/04/13 ********************************************
     D #rtcd           s              1
     D*** End Dlt *** 01/04/13 ********************************************
    D***** Beg Add *** 05/22/19 ***************************
     D inEOrrn         s              9    inz
     D inMode          s              1    inz
     D inTabInd        s              5    inz
    D***** End Add *** 05/22/19 ***************************
     d*** Beg Add *** 03/19/24 *********************************************
     d isOvr           s              1    Inz('Y')
     d strCmd          s            256    Inz
     d inAdjState      s                   Like(EOast)
     d*** End Add *** 03/19/24 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*** Beg Add ****** 05/03/19 **************************
     d DtlChgsList     pr                  ExtPgm('MTRPTDTCHP')
     d                                3  0
     d                                3  0
     d                                7  0
     d                                9  0
     d                                3  0
    D*** End Add ****** 05/03/19 **************************
    D***** Beg Add *** 05/22/19 ***************************
     d PBDetails       pr                  ExtPgm('FTPBDTL')
     d inEOrrn                        9
     d inMode                         1
     d inTabInd                       5    options(*nopass)
    D***** End Add *** 05/22/19 ***************************
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
    D*** Beg Add ****** 11/22/16 **************************
      /Copy sPrModule
    D*** End Add ****** 11/22/16 **************************
    D*** Beg Add ****** 03/19/24 **************************
      /Copy sPrRunCmd
    D*** End Add ****** 03/19/24 **************************

     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@eob#
     C                   parm                    @@cert
     C                   parm                    @@sfdt
     C                   parm                    @@stdt
     C                   parm                    @@sfei
     C                   parm                    @@smod
     C                   parm                    @@rcn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    #2sub#
     C                   parm                    #2fnd
     C                   parm                    #2fyr
     C                   parm                    #2case
     C                   parm                    ##nwst
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     chgpl         plist
     C                   parm                    qqnum
     C                   parm                    qqmask
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*
    C* PARM LIST FOR DARTS API
    C*
     C     drtspl        plist
     C                   parm      *blanks       #1rtcd
     C                   parm      ##wcin        #1wcin
     C                   parm      ##wcdk        #1wdck
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      *blanks       ##key
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     rstpl         plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr            10
     C                   parm      'M'           ##appl            1
     C                   parm      *off          ##rcds            1
    C*
     C     rstplf        plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C                   parm                    ##auth
     C                   parm      fnfnd         ##fnd
     C                   parm      *zero         ##emp#            9 0
     C                   parm      fnsub#        ##sub#
    C*
     C     rstpls        plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C                   parm                    ##auth
     C                   parm      *zero         ##fnd
     C                   parm      *zero         ##emp#
     C                   parm      smsub#        ##sub#
    C*
     C     qrypl         plist
     C                   parm                    qryslt
    C*
    C*** parameter list for claims secuirty API
    C*
     C     cuspl         plist
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      q1user        ##usr
     C                   parm                    #1rtcd
    C*
    C* key lists for eomb transaction file
    C*
     C     key           klist
     C                   kfld                    ##co#
    C*
     C     key1          klist
     C                   kfld                    ##co#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key2          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key3          klist
     C                   kfld                    ##co#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key4          klist
     C                   kfld                    ##co#
     C                   kfld                    kkeob#
    C*
     C     key5          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key6          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key7          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key8          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    kkeob#
    C*
     C     key9          klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
    C*
     C     key10         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key11         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
    C*
     C     key11a        klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key12         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key13         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key14         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key15         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
    C*
     C     key15a        klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key16         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key17         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*
     C     key18         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key19         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key20         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key21         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
     C                   kfld                    kkeob#
    C*
     C     key22         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sscert
    C*
     C     key23         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key24         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key25         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key26         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkeob#
    C*
     C     key27         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
    C*
     C     key28         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
    C*
     C     key29         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     key30         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
    C*
     C     key31         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    sseob#
    C*
     C     key32         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
    C*
     C     key33         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     key34         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
    C*
     C     key35         klist
     C                   kfld                    ##co#
     C                   kfld                    sseob#
    C*
     C     key36         klist
     C                   kfld                    ##co#
     C                   kfld                    ssblst
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key37         klist
     C                   kfld                    ##co#
     C                   kfld                    ssblst
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key38         klist
     C                   kfld                    ##co#
     C                   kfld                    ssblst
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key39         klist
     C                   kfld                    ##co#
     C                   kfld                    ssblst
     C                   kfld                    kkeob#
    C*
     C     key40         klist
     C                   kfld                    ##co#
     C                   kfld                    ssblst
    C*
     C     key41         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkcntl
    C*
     C     key42         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkcert
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key43         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkfein
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key44         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key45         klist
     C                   kfld                    ##co#
     C                   kfld                    kkfein
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key46         klist
     C                   kfld                    ##co#
     C                   kfld                    kkeob#
     C                   kfld                    kkcntl
    C*
     C     key47         klist
     C                   kfld                    ##co#
     C                   kfld                    ssblst
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key48         klist
     C                   kfld                    ##co#
     C                   kfld                    ssiusr
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key49         klist
     C                   kfld                    ##co#
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key50         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key51         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key52         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key53         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkeob#
    C*
     C     key54         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key55         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
    C*
     C     key56         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key57         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key58         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key59         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkeob#
    C*
     C     key60         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key61         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
    C*
     C     key62         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
     C                   kfld                    kkfrdt
     C                   kfld                    kkeob#
    C*
     C     key63         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkeob#
    C*
     C     key64         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key65         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkeob#
    C*
     C     key66         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    kkcntl
     C                   kfld                    kkeob#
    C*
     C     key67         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
    C*
     C     key68         klist
     C                   kfld                    ##co#
     C                   kfld                    kkidoc
     C                   kfld                    kkeob#
    C*
    C* key list for provider demographics file
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     pdky1         klist
     C                   kfld                    kkfein
    C*
    C* key list for claims master file
    C*
     C     cmky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
     C     cmky1         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
    C*
    C* key list for eomb status change transaction file
    C*
     C     esky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     esky2         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkidt
     C                   kfld                    kkitim
    C*
    C* key list for pre-cert master file
    C*
     C     pcky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkcert
     C                   kfld                    kkext
    C*
    C* key list for subscriber master file
    C*
     C     smky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
    C*
    C* key list for WCMC fund/group master file
    C*
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
    C*
     C     fnkey         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C*
     C     fnky3         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
    C*
    C* key list for generic table file
    C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*
    C     upky          klist
    C                   kfld                    kkusr
    C*
     C     upky2         klist
     C                   kfld                    kkstaf
    C*
    C* key list for Bill Entry Control File
    C*
     C     beky          klist
     C                   kfld                    scast
     C                   kfld                    scsys
     C                   kfld                    scbtyp
    C*
     C     eaky          klist
     C                   kfld                    ##co#
     C                   kfld                    scsub#
     C                   kfld                    scfnd
     C                   kfld                    scfyr
     C                   kfld                    sccase
     C                   kfld                    sceob#
     C                   kfld                    scrcn
    C*
     C     eaky2         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
    C*
     C     eaky3         klist
     C                   kfld                    ##co#
     C                   kfld                    ssidoc
    C*
     C     doky          klist
     C                   kfld                    kkdcid
    C*
     C     dtky          klist
     C                   kfld                    kkdoct
    C*
     C     XRKY          KLIST
     C                   kfld                    ##co#
     C                   kfld                    Q1USER
     C                   kfld                    scfnd
     C                   kfld                    scfyr
     C                   kfld                    sccase
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*================================================================
    C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
    C*================================================================
    C*
     C     assrrn        begsr
    C*
B01  C                   select
    C*
S01  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnu
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnv
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrnw
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrng
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr07
E02  C                   endif
    C*
S02  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnm
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnn
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrno
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrnp
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr08
E02  C                   endif
    C*
S03  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnh
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrni
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr06
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrni
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr09
E02  C                   endif
    C*
S04  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnc
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnd
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrne
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrng
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr10
E02  C                   endif
    C*
S05  C                   when      ssfein <> *zeros
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnk
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr05
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrnl
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr05
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr11
E02  C                   endif
    C*
S06  C                   when      sseob# <> *zeros
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnx
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrny
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrnz
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrn4
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr12
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C                   eval      scdbrr = dbrrnz
E02  C                   endif
    C*
S07  C                   when      sssub# <> *zeros and ssfnd <> *zeros
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr25
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr26
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr27
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr28
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr29
E02  C                   endif
     C*
S08  C                   when      sssub# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr20
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr21
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr22
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr23
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr24
E02  C                   endif
    C*
S09  C                   when      ssblst <> *blanks
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr01
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr02
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr03
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr04
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr13
E02  C                   endif
    C*
S10  C                   when      ssbtyp <> *blanks
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr15
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr16
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr17
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr18
    C*
E02  C                   endif
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr19
E02  C                   endif
    C*
S11  C                   when      ssiusr <> *blanks
    C*
B02  C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnq
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnr
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrns
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrnt
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr14
E02  C                   endif
    C*
E01  C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
    C*
     C     chkssm        begsr
    C*
B01  C                   if        sssub# <> *zeros
     C                             or ssfnd <> *zeros
     C                             or ssfyr <> *zeros
     C                             or sscase <> *zeros
     C                             or sseob# <> *zeros
     C                             or sscert <> *blanks
     C                             or ssfrdt <> *zeros
     C                             or sstodt <> *zeros
     C                             or ssfein <> *zeros
     C*** Beg Chg *** 07/26/16 ***************************************
     C**************************   or ssmod <> *blanks
     C                             or ssmod <> *zeros
     C*** End Chg *** 07/26/16 ***************************************
     C                             or ssbtyp <> *blanks
     C                             or ssidoc <> *blanks
    C*
     C                   eval      *in11 = *on
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* DATSBR - Date check subrountine
    C*================================================================
    C*
     C     datsbr        begsr
    C*
    C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
    C* FORMAT ALL DATES ON SCREEN
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssfrdt
    C*
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sstodt
    C*
     C                   endsr
    C*
    C*================================================================
    C* DLOVRD - Delete file overrides
    C*================================================================
    C*
     C     dlovrd        begsr
    C*
     C***  Begin Chg  ***  06/27/12  **************************************
     C**********         eval      length = 56
     C                   eval      length = 60
     C***  End   Chg  ***  06/27/12  **************************************
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(8)
     C                   parm                    length
    C*
    C*** BEG CHG *** 01/09/13 ****************************************
    C*
     C*****              call      'QCMDEXC'                              60
     C                   call(e)   'QCMDEXC'                              60
    C*
    C*** END CHG *** 01/09/13 ****************************************
    C*
     C                   parm                    qca(9)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(10)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(11)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(12)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(13)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(14)
     C                   parm                    length
    C*
     C                   endsr
    C*
    C*================================================================
    C* DSPDOC - Display Document
    C*================================================================
    C*
     C     dspdoc        begsr
     C*
     C                   movel(p)  scidoc        kkdcid
    C*
     C     doky          chain     pdo01                              99
    C*
B01  C                   if        *in99 = *off
    C*
B02  C                   if        dofl01 = 'F'
     C*
     C* Call the DSPFAXC Program to display the fax, else
     C*
     C                   movel     doflr         xxflr             8
     C                   movel     dosflr        xxsflr           12
     C     xxflr         cat       '/'           xx9a              9
     C     xx9a          cat(p)    xxsflr        $$fold
     C*
     C                   call      'DSPFAXC'
     C                   parm      scidoc        $$dcid           12
     C                   parm                    $$FOLD           80
     C                   parm      *blanks       $$rtrn            1
     C*
X02  C                   else
     C*
     C* Image Display
     C*
     C                   movel(p)  dodtyp        kkdoct
    C*
     C     dtky          chain     pdt01                              99
    C*
B03  C                   if        *in99 = *off
     C                   eval      $$desc = dtdesc
X03  C                   else
     C                   eval      $$desc = '*N/A*'
E03  C                   endif
     C*
     C                   call      'IS211C'
     C                   parm      scidoc        $$DCID
     C                   parm      doflr         $$FLR             8
     C                   parm      dosflr        $$SFLR           12
     C                   parm      dodtyp        $$DOCT            8
     C                   parm                    $$DESC           40
     C                   parm      *blanks       $$RTN             6
     C*
E02  C                   endif
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
    C*
     C     errck1        begsr
    C*
    C* If authority for group is 'I' then check for the authority again while entering the bill to
    C* confirm the restriction.
    C*
B01  C                   if        scauth = 'I'
     C                             and *in55 = *on
    C*
B02  C                   if        scsel = '2'
     C                             or scsel = '3'
     C                             or scsel = '4'
     C                             or scsel = '6'
    C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      scfnd         $$fnd
     C                   parm      *zero         #2emp#
     C                   parm      scsub#        $$sub#
    C*
E02  C                   endif                                                  scsel
E01  C                   endif                                                  scauth
    C*
     C                   move      *OFF          *IN45
    C*
B01  C                   if        WMXRLopen = *on
    C*
     C     xrky          chain     pxr01                              60
    C*
B02  C                   If        not *in60 AND XRSTAT = 'A' AND
     C                             SCSEL <> *blanks
     C                   Move      *ON           *IN45
     C                   Move      *ON           *IN90
E02  C                   EndIf
    C*
E01  C                   endif
    C*
     C                   move      'Y'           vldopt
    C*
B01  C                   select
    C*
    C*  Check for user authorization to OPT 2 = change.
    C*
S01  C                   when      scsel = '2'
     C                             and scauth = 'I'
     C                             and ##rcds = *on
     C                             and *in55 = *on
    C*
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
    C*
S02  C                   when      scauth = 'I'
     C                             and *in55 = *on
    C*
B02  C                   if        scsel = '3'
     C                             or scsel = '4'
     C                             or scsel = '6'
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
E02  C                   endif
    C*
S03  C                   other
    C*
    C* disallow certain options of bill status is delete
    C*
B02  C                   if        scblst = 'DEL'
    C*
B03  C                   if        scsel = '2'
     C                             or scsel = '4'
     C                             or scsel = '9'
     C                             or scsel = '12'
     C                             or scsel = '13'
     C                             or scsel = '17'
     C                             or scsel = '19'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck1end
E03  C                   endif
    C*
E02  C                   endif
    C*
B02  C                   if        scblst = 'DEL'
     C                             and @@call <> '1'
     C                             and scsel = '7'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck1end
E02  C                   endif
    C*
    C* DO NOT CALL OPT15=PRE-CERT INFO IF NO PRECERT EXIST FOR BILL
    C*
B02  C                   if        scsel = '15'
    C*
    C**** Beg Chg *** 02/03/17 ****************************************
     C**** scdbrr        chain     pfrcdf                             98
     C     scdbrr        chain(n)  pfrcdf                             98
    C**** End Chg *** 02/03/17 ****************************************
    C*
B03  C                   if        *in98 = *off
     C                             and eocert = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   goto      ck1end
E03  C                   endif
    C*
E02  C                   endif
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '2' or scsel = '4'
     C*
     C     eaky          chain     pea01                              99
    C*
B04  C                   if        *in99=*off and eansnt = 'S'
     C                   seton                                        7590
E04  C                   endif
     C*
E03  C                   endif
    C*
E02  C                   endif
    C*
     C*** Beg Chg *** 04/17/13 ************************************************
     C********           if        ##clt# = '146'
B02  C                   if        ##clt# = '146' or ##clt# = '114'
     C*** End Chg *** 04/17/13 ************************************************
    C*
B03  C  N90              if        scsel = '2' or scsel = '4'
     C*
     C     eaky          chain     pea01                              99
    C*
B04  C                   if        *in99=*off and eansnt = 'S'
     C                   seton                                        7590
E04  C                   endif
     C*
E03  C                   endif
    C*
E02  C                   endif
     C*
     C*** Beg Add *** 01/25/17 ****************************************
     C* If the claim is locked then give an error message
     C                   setoff                                       44
     C                   if        scSel = '2'
     C     cmky1         chain     plo01                              99
     C                   if        *in99 = *off
     C                   eval      *in90 = *on
     C*** Beg Chg *** 11/15/17 ******************************************
     C****************** eval      *in44 = *on
    C                   eval      errClmLck = '1'
     C*** End Chg *** 11/15/17 ******************************************
     C                   goto      ck1end
     C                   Endif
     C                   Endif
     C*** End Add *** 01/25/17 ****************************************
    C*
    C* If FL Medical EDI bill submission has been made, and bill has not
    C* been Withdrawn, display error message to withdraw bill before delete
    C* will be allowed.
    C*
B02  C                   if        scsel = '4'
     C*
B03  C                   if        eotape <> 'N'
     C*
B04  C                   if        eoast = 'FL'
     C                             and eotape <> *blank
     C                             and eotape <> 'E'
     C                             and eotape <> 'W'
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   goto      ck1end
E04  C                   endif
     C*
E03  C                   endif
     C*
E02  C                   endif
     C*
     D*** Beg Dlt *** 01/04/13 ********************************************
     C**N90              if        ##clt# = '114' and
     C***                          (scsel = '2' or scsel ='4') and
     C***                          eoblst ='C' and eofnd = 461
     C*
     C***                call      'FA773R'
     C***                parm      scdbrr        ##rrn
     C***                parm      *blanks       #rtcd
     C*
     C***                if        #rtcd = 'Y'
     C***                exfmt     wdwerr
     C***                seton                                        90
     C***                endif
     C*
     C***                endif
     D*** End Dlt *** 01/04/13 ********************************************
    C*
    C* DELETE PROCESSING
    C*
B02  C                   if        *in90 = *off
     C                   eval      ##rrn = 0
B03  C                   if        scsel = '4'
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
    C*
     C                   call      'FA701R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       ##outs
    C*
    C* Only chain to WDPAP if Workers Comp is inst and linked case
    C*
B04  C                   if        ##wcin = 'Y'
     C                             and ##rtcd = *blank
     C                             and ##outs <> 'Y'
     C                             and ##npwc <> 'Y'
     C                             and ##wcif = 'Y'
    C*
    C* When there is a check in history only allow option 4=Delete
    C* if the user has the proper security level.
    C*
     C                   move      ##co#         #1co#
     C                   move      scfnd         #1fnd
     C                   move      scsub#        #1sub#
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
     C                   move      sceob#        #1eob#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
     C                   call      'WA047R'
     C                   parm                    #1co#
     C                   parm      #1wcf#        #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    #1eob#
     C                   parm      *blanks       ##rtcd
    C*
B05  C                   if        ##rtcd <> 'F'
    C*
     C                   call      'FA715R'
     C                   parm      ' '           ##rtcd
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    ##pvmc
     C                   parm                    ##pvpr
     C                   parm                    ##wcup
     C                   parm                    ##cldl
     C                   parm                    ##cldt
     C                   parm                    ##cltl
     C                   parm                    ##cltt
     C                   parm                    ##sdcl
     C                   parm                    ##sdct
     C                   parm                    ##bscl
     C                   parm                    ##bsct
     C                   parm                    ##dcdl
     C                   parm                    ##dcdt
     C                   parm                    ##dccl
     C                   parm                    ##dcct
     C                   parm                    ##c#cl
     C                   parm                    ##c#ct
     C                   parm                    ##fimr
     C                   parm                    ##timr
     C                   parm                    ##frn
     C                   parm                    ##trn
     C                   parm                    ##cs#l
     C                   parm                    ##cs#t
     C                   parm                    ##dnsl
     C                   parm      ' '           ##dnst
     C                   parm                    ##nerc
     C                   parm                    ##meus
     C                   parm                    ##nwpa
     C                   parm                    ##cpaa
     C                   parm                    ##cpad
     C                   parm                    ##plcl
     C                   parm                    ##plct
     C                   parm      0             ##ckdl
     C                   parm      ' '           ##ckdt
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
B06  C                   if        ##rtcd = *blanks
     C                   call      'SA006R'
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##ckdl
     C                   parm                    ##ckdt
     C                   parm      ' '           ##rtcd
    C*
B07  C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in88 = *on
     C                   goto      ck1end
E07  C                   endif
    C*
E06  C                   endif
    C*
E05  C                   endif
    C*
E04  C                   endif
    C*
    C*
    C* IF AR IS INSTALLED, CALL WORK WITH EOMB INVOICES PROGRAM
    C*
B04  C                   if        ##ar = 'Y'
     C                   eval      ##rrn = scdbrr
     C                   move      ' '           ##call
     C                   eval      ##sub# = scsub#
     C                   eval      ##fnd = scfnd
     C                   eval      ##eob# = sceob#
     C                   eval      ##case = sccase
     C                   eval      ##fyr = scfyr
    C*
     C                   call      'N0350R1'
     C                   parm                    ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm                    ##co#
     C                   parm                    ##sub#
     C                   parm                    ##fnd
     C                   parm                    ##fyr
     C                   parm                    ##case
     C                   parm                    ##eob#
    C*
B05  C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                   move      '1'           rstpgm
     C                   clear                   scsel
     C                   move      *on           stop
     C                   goto      ck1end
E05  C                   endif
    C*
E04  C                   endif
    C*
     C                   movel     '99'          scsel
    C*
E03  C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IF THE CORRESPONDING
    C* GROUP IS EXPIRED  (F9, F19)
    C*
B03  C                   if        scsel = '9'
     C                             or scsel = '19'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
    C*
     C                   call      'FA729R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    #2err
    C*
B04  C                   if        #2err = '1'
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck1end
E04  C                   endif
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IN CHANGE MODE IF THE
    C* CORRESPONDING GROUP IS EXPIRED.  IF ONE OF THESE OPTIONS IS
    C* PRESSED, THEN GIVE THE USER THE EXPIRED GROUP WINDOW AND TAKE
    C* THEM INTO THEIR SELECTED OPTION IN DISPLAY MODE.
    C*          OPTIONS 2 AND 18
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '2'
     C                             or scsel = '18'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
    C*
     C                   call      'FA729R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    #2err
    C*
B04  C                   if        #2err = '1'
    C*
     C                   call      'FA720R'
     C                   parm      '2'           ##mgid
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
    C*
B05  C                   if        scsel = '2'
     C                   movel(p)  '5'           scsel
E05  C                   endif
    C*
     C                   move      'D'           ##mode
     C                   goto      ck1end
E04  C                   endif
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IF THE
    C* CLAIM INDICATES A 'Y' IN THE DENIED FLAG AND THE USER'S
    C* SECURITY LEVEL AND TYPE DOES NOT PASS THE SECURTY LEVEL AND
    C* TYPE DEFINED INTHE CLAIMS/DARTS CONTROL FILE.
    C*          OPTIONS 9 AND 19
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '9'
     C                             or scsel = '19'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
    C*
B04  C                   if        *in99 = *off
    C*
B05  C                   if        cmdny = 'Y'
     C                   eval      ##dnsl = 0
    C*
     C                   call      'FA715R'
     C                   parm      ' '           ##rtcd
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    ##pvmc
     C                   parm                    ##pvpr
     C                   parm                    ##wcup
     C                   parm                    ##cldl
     C                   parm                    ##cldt
     C                   parm                    ##cltl
     C                   parm                    ##cltt
     C                   parm                    ##sdcl
     C                   parm                    ##sdct
     C                   parm                    ##bscl
     C                   parm                    ##bsct
     C                   parm                    ##dcdl
     C                   parm                    ##dcdt
     C                   parm                    ##dccl
     C                   parm                    ##dcct
     C                   parm                    ##c#cl
     C                   parm                    ##c#ct
     C                   parm                    ##fimr
     C                   parm                    ##timr
     C                   parm                    ##frn
     C                   parm                    ##trn
     C                   parm                    ##cs#l
     C                   parm                    ##cs#t
     C                   parm                    ##dnsl
     C                   parm      ' '           ##dnst
     C                   parm                    ##nerc
     C                   parm                    ##meus
     C                   parm                    ##nwpa
     C                   parm                    ##cpaa
     C                   parm                    ##cpad
     C                   parm                    ##plcl
     C                   parm                    ##plct
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
B06  C                   if        ##rtcd = *blanks
    C*
     C                   call      'SA006R'
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##dnsl
     C                   parm                    ##dnst
     C                   parm      ' '           ##rtcd
    C*
B07  C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck1end
E07  C                   endif
    C*
E06  C                   endif
    C*
E05  C                   endif
    C*
E04  C                   endif
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IN CHANGE MODE IF THE
    C* CLAIM INDICATES A 'Y' IN THE DENIED FLAG AND THE USER'S
    C* SECURITY LEVEL AND TYPE DOES NOT PASS THE SECURTY LEVEL AND
    C* TYPE DEFINED INTHE CLAIMS/DARTS CONTROL FILE.
    C*          OPTIONS 2, 17 AND 18
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '2'
     C                             or scsel = '18'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
    C*
B04  C                   if        *in99 = *off
    C*
B05  C                   if        cmdny = 'Y'
     C                   eval      ##dnsl = 0
    C*
     C                   call      'FA715R'
     C                   parm      ' '           ##rtcd
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    ##pvmc
     C                   parm                    ##pvpr
     C                   parm                    ##wcup
     C                   parm                    ##cldl
     C                   parm                    ##cldt
     C                   parm                    ##cltl
     C                   parm                    ##cltt
     C                   parm                    ##sdcl
     C                   parm                    ##sdct
     C                   parm                    ##bscl
     C                   parm                    ##bsct
     C                   parm                    ##dcdl
     C                   parm                    ##dcdt
     C                   parm                    ##dccl
     C                   parm                    ##dcct
     C                   parm                    ##c#cl
     C                   parm                    ##c#ct
     C                   parm                    ##fimr
     C                   parm                    ##timr
     C                   parm                    ##frn
     C                   parm                    ##trn
     C                   parm                    ##cs#l
     C                   parm                    ##cs#t
     C                   parm                    ##dnsl
     C                   parm      ' '           ##dnst
     C                   parm                    ##nerc
     C                   parm                    ##meus
     C                   parm                    ##nwpa
     C                   parm                    ##cpaa
     C                   parm                    ##cpad
     C                   parm                    ##plcl
     C                   parm                    ##plct
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
B06  C                   if        ##rtcd = *blanks
    C*
B07  C                   if        ##lvl < ##dnsl
     C                             or ##type <> ##dnst
    C*
     C                   call      'FA720R'
     C                   parm      '3'           ##mgid
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
    C*
B08  C                   if        scsel = '2'
     C                   movel(p)  '5'           scsel
E08  C                   endif
    C*
     C                   move      'D'           ##mode
     C                   goto      ck1end
E07  C                   endif
    C*
E06  C                   endif
    C*
E05  C                   endif
    C*
E04  C                   endif
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
    C*          Option 3 = Copy
    C*             Call F7098 to perform copies
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '3'
    C*
     C                   call      'F7098R'
     C                   parm      scdbrr        @3rrn
     C                   parm                    @3rrn2
     C                   parm                    @3rrn3
    C*
    C*             Call F0395 to display new EOMB
    C*
     C                   move      'D'           @3mode
     C                   move      'P'           @3blst
    C*
     C                   call      'F0395R01'
     C                   parm      *inkc         @3kc
     C                   parm      *inkl         @3kl
     C                   parm                    @3mode
     C                   parm                    @3rrn2
     C                   parm                    @3rrn3
     C                   parm                    @3blst
     C                   parm      *blanks       @3stcd
     C                   parm      *blanks       @3reas
     C                   parm      *blanks       @3pgm
     C                   clear                   scsel
     C                   eval      *in90 = *on
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
    C*
    C* DETERMINE IF THE OPTION2=CHANGE IS A REGULAR CHANGE OR TAPE
    C* SUBMISSION CORRECTION OR EDI CORRECTION AND TAKE THE
    C* APPROPRIATE PATH
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '2'
    C*
    C**** Beg Chg *** 02/03/17 ****************************************
     C**** scdbrr        chain     pfrcdf                             99
     C     scdbrr        chain(n)  pfrcdf                             99
    C**** End Chg *** 02/03/17 ****************************************
    C*
B04  C                   if        *in99 = *off
     C                             and eoblst = 'C'
    C*
B05  C                   if        eotape = 'Y'
     C                             or eotape = 'R'
     C                             or eotape = 'C'
     C                             or eotape = 'S'
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   eval      kkrcn = eorcn
     C                   eval      ##sts = ' '
    C*
B06  C                   if        eoast <> 'KY'
    C*
     C                   call      'FA731R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm      kkfnd         ##fnd
     C                   parm      kkfyr         ##fyr
     C                   parm      kkcase        ##case
     C                   parm      kkeob#        ##eob#
     C                   parm      kkrcn         ##rcn
     C                   parm                    ##lseq
     C                   parm                    ##sts
E06  C                   endif
    C*
B06  C                   if        ##sts <> '  '
    C*
B07  C                   if        ##sts = 'SN'
    C*
     C                   eval      *in90 = *on
     C                   eval      *in87 = *on
     C                   goto      ck1end
E07  C                   endif
    C*
     C***** Beg Chg ***** 03/25/14 ******************************
B07  C*****              if        ##sts = 'TA'
B07  C                   if        ##sts = 'TA' or ##sts = 'IA'
     C***** End Chg ***** 03/25/14 ******************************
     C                   move      'R'           ##nwst
X07  C                   else
     C                   move      'E'           ##nwst
E07  C                   endif
    C*
X06  C                   else
    C*
B07  C                   if        eocntl = *blanks
     C                   move      'R'           ##nwst
X07  C                   else
     C                   move      'B'           ##nwst
E07  C                   endif
    C*
E06  C                   endif
    C*
X05  C                   else
     C                   move      'R'           ##nwst
E05  C                   endif
    C*
E04  C                   endif
    C*
E03  C                   endif
    C*
E02  C                   endif
     C*
     C*** Beg Add *** 06/15/12 *********************************************
     C*
B02  C                   if        *in90 <> *on and ##clt# = '114'
     C                             and eocrne = 'D'
     C*
     C                   eval      *in90 = *on
     C                   eval      scsel = *blanks
     C*
     C                   exfmt     wdwerr2
     C*
E02  C                   endif
     C*
     C*** End Add *** 06/15/12 *********************************************
    C*
    C* DON'T ALLOW A TWO TO CHANGE IF CODE REVIEW PENDED THIS ERROR
    C* DUE TO AN ERROR IN PROCESSING.
    C*
B02  C                   if        *in90 = *off
    C*
B03  C                   if        scsel = '2'
     C                             and sccrpe = 'E'
    C*
     C                   exsr      mvdb04
     C                   exfmt     scr04
     C                   seton                                        9084
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
E01  C                   endsl
     C*
     C* If a document is associated with this eomb the display it.
     C*
     C*** Begin Chg *** 04/10/19 ***********************************
B01  C**********         if        ##clt# = '129' or ##clt# = '114'
B01  C                   if        ##clt# = '129' or ##clt# = '114' or
     C                             ##clt# = '152'
     C*** End   Chg *** 04/10/19 ***********************************
       //*** Beg Add *** 06/26/19 ******************************************
      /Free
                         If scIdoc = *blanks and scSel = '2';
                            Chain (EOeob#:EOrcn) pu101;
                            If %Found(fau1l2) and U1sDoc <> *blanks;
                               Chain (U1sDoc) pop01;
                               If %Found(optimptl2);
                                  EOiDoc = OPsDoc;
                                  scIdoc = EOiDoc;
                               Endif;
                            Endif;
                         Endif;
      /End-Free
       //*** End Add *** 06/26/19 ******************************************
    C*
B02  C                   if        scsel = '2' and scidoc <> *blanks
     C                   exsr      dspdoc
E02  C                   endif
    C*
E01  C                   endif
    C*
     C     ck1end        endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
    C*
     C     errck2        begsr
    C*
    C* PROCESS SUBSET SEARCHES
    C*
     C                   exsr      othfks
    C*
     C     *in90         cabeq     *on           ck2end
    C*
    C* CLEAR SUBSET DESCRIPTION FIELDS
    C*
     C                   exsr      sscler
    C*
    C* START OF ERROR CHECKING
    C*
B01  C                   if        ssfnd <> *zeros
     C                             and sssub# = *zeros
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   goto      ck2end
E01  C                   endif
    C*
B01  C                   if        sssub# = *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck2end
E01  C                   endif
    C*
B01  C                   if        ssfnd = *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   goto      ck2end
E01  C                   endif
    C*
B01  C                   if        ssfyr = *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck2end
E01  C                   endif
    C*
B01  C                   if        ssfein = *zeros
     C*** Beg Chg *** 07/26/16 ***************************************
     C*************************    and ssmod <> *blanks
     C                             and ssmod <> *zeros
     C*** End Chg *** 07/26/16 ***************************************
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck2end
E01  C                   endif
    C*
    C* validate subscriber number
    C*
B01  C                   if        sssub# <> *zeros
     C                   eval      kksub# = sssub#
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
B02  C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in75 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate group
    C*
B01  C                   if        ssfnd <> *zeros
     C                   eval      kkfnd = ssfnd
     C                   eval      kksub# = sssub#
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm                    #2err
    C*
B02  C                   if        #2err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in76 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate case
    C*
B01  C                   if        sscase <> *zeros
     C                   eval      kksub# = sssub#
     C                   eval      kkfnd = ssfnd
     C                   eval      kkfyr = ssfyr
     C                   eval      kkcase = sscase
     C     cmky          chain     pcm01                              99
B02  C                   if        *in99 = *on
     C                   eval      *in90 = *on
     C                   eval      *in77 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate provider
    C*
B01  C                   if        ssfein <> *zeros
     C*** Beg Chg *** 07/26/16 ***************************************
     C************************     and ssmod <> *blanks
     C                             and ssmod <> *zeros
     C*** End Chg *** 07/26/16 ***************************************
     C                   eval      kkfein = ssfein
     C                   movel     ssmod         kkmod
    C*
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
B02  C                   if        #1err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck2end
X02  C                   else
E02  C                   endif
    C*
E01  C                   endif
    C*
B01  C                   if        ssfein <> *zeros
     C*** Beg Chg *** 07/26/16 ***************************************
     C***********************      and ssmod = *blanks
     C                             and ssmod <> *zeros
     C*** End Chg *** 07/26/16 ***************************************
     C                   eval      kkfein = ssfein
    C*
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C*** Beg Chg *** 06/11/15 ********************************************
     C****************   parm                    #1mod
     C                   parm      *zeros        #1mod
     C*** End Chg *** 06/11/15 ********************************************
     C                   parm                    #1name
     C                   parm                    #1err
    C*
B02  C                   if        #1err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck2end
X02  C                   else
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate dates of service
    C*
B01  C                   if        ssfrdt <> *zeros
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
    C*
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssfrdt
    C*
B02  C                   if        srerr = '1'
     C                   eval      *in90 = *on
     C                   eval      *in79 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
B01  C                   if        sstodt <> *zeros
    C*
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
    C*
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sstodt
    C*
B02  C                   if        srerr = '1'
     C                   eval      *in90 = *on
     C                   eval      *in83 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* From Service Date cannot be greater than To Service Date
    C*
B01  C                   if        ssfrdt > *zeros
     C                             and sstodt > *zeros
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
    C*
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlfrdt
    C*
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
    C*
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hltodt
    C*
B02  C                   if        hlfrdt > hltodt
     C                   eval      *in90 = *on
     C                   eval      *in84 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate bill status
    C*
B01  C                   if        ssblst <> *blanks
     C                   movel(p)  ssblst        kkkey
     C                   movel     'BLST'        kktbid
    C*
     C                   call      'FA728R'
     C                   parm                    ##co#
     C                   parm      kktbid        ##tbid
     C                   parm      kkkey         #1key
     C                   parm                    ##desc
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
B02  C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in80 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate bill type
    C*
B01  C                   if        ssbtyp <> *blanks
     C                   movel(p)  ssbtyp        kkkey
     C                   movel     'BTYP'        kktbid
    C*
     C                   call      'FA728R'
     C                   parm                    ##co#
     C                   parm      kktbid        ##tbid
     C                   parm      kkkey         #1key
     C                   parm                    ##desc
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
B02  C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in89 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* validate initial user
    C*
B01  C                   if        ssiusr <> *blanks
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      ssiusr        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
B02  C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in81 = *on
     C                   goto      ck2end
E02  C                   endif
    C*
E01  C                   endif
     C*
     C* Validate the document id
     C*
B01  C                   if        ssidoc<>*blanks
    C*
     C     eaky3         chain     pea02
    C*
B02  C                   if        %found
     C                   eval      sseob#=eaeob#
     C                   clear                   ssidoc
X02  C                   else
     C                   seton                                        8890
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* CALL DATE FORMATTING SUBROUTINE
    C*
     C                   exsr      datsbr
    C*
     C     ck2end        endsr
    C*
    C*===============================================================
    C* GETGRP - SUBROUTINE TO GET GROUPS USER IS AUTHORIZED TO
    C*===============================================================
    C*
     C     getgrp        begsr
    C*
     C                   eval      x = 1
     C                   move      *off          grperr
    C*
     C     fnky3         chain     pfn02                              60
    C*
B01  C                   dow       *in60 = *off
    C*
     C                   call      'SA008R'      rstplf
    C*
B02  C                   if        ##rtcd = *off
     C                   move      fnfnd         grp(x)
     C                   eval      x = x + 1
X02  C                   else
     C                   move      *on           grperr
E02  C                   endif
    C*
     C     fnky3         reade     pfn02                                  60
    C*
E01  C                   enddo
    C*
B01  C                   if        grperr = *on
    C*
     C                   cat       fnd:1         qryslt
    C*
B02  C                   do        x             z
     C                   cat       grp(z):1      qryslt
E02  C                   enddo
    C*
     C                   cat       '))':0        qryslt
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*===============================================================
    C* GETSUB - SUBROUTINE TO GET SUBSCRIBERS USER IS AUTHORIZED TO
    C*===============================================================
    C*
     C     getsub        begsr
    C*
     C     ##co#         chain     psm01                              60
    C*
     C                   move      *on           first
     C                   move      ##co#         xaco#
    C*
B01  C                   dow       *in60 = *off
    C*
     C                   call      'SA008R'      rstpls
    C*
B02  C                   if        ##rtcd = *off
    C*
     C                   eval      ##sub# = smsub#
     C                   eval      kksub# = smsub#
     C                   move      smsub#        xasub#
    C*
B03  C                   if        first = *on
     C                   move      *off          first
     C     co#           cat(p)    xaco#:1       qryslt
     C                   cat       co#2:0        qryslt
     C                   cat       sub#:0        qryslt
     C                   cat       xasub#:1      qryslt
     C                   cat       ')':0         qryslt
X03  C                   else
     C                   cat       '*OR':1       qryslt
     C                   cat       sub#:1        qryslt
     C                   cat       xasub#:1      qryslt
     C                   cat       ')':0         qryslt
E03  C                   endif
    C*
     C                   clear                   grp
    C*
B03  C                   if        ##mrgr = 'Y'
     C                   exsr      getgrp
E03  C                   endif
    C*
E02  C                   endif
    C*
     C     ##co#         reade     psm01                                  60
    C*
E01  C                   enddo
    C*
     C                   cat       ')':0         qryslt
    C*
    C*  CALL CL TO CREATE AN OPEN QUERY FILE OVER THE FILES
    C*
     C     ovrdb1        cat(p)    'FAEOLH':0    xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      31            length
    C*
     C     ovrdb1        cat(p)    'FAEOLI':0    xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      31            length
    C*
     C     ovrdb1        cat(p)    'FAEOLJ':0    xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      31            length
    C*
     C     ovrdb1        cat(p)    'FAEOLK':0    xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      32            length
    C*
     C     ovrdb1        cat(p)    'FAEOL32':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      44            length
    C*
     C     ovrdb1        cat(p)    'FAEOL75':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      44            length
    C*
     C                   call      qrypgm        qrypl                  99
    C*
     C                   endsr
    C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
    C*
     C     inzsst        begsr
    C*
     C                   eval      #2sub# = 0
     C                   eval      #2fnd = 0
     C                   eval      #2fyr = 0
     C                   eval      #2case = 0
    C*
B01  C                   if        q1prms > 5
     C                             and @@sub# <> *zeros
     C                             and @@fnd <> *zeros
     C                             and @@fyr <> *zeros
     C                             and @@case <> *zeros
     C                   eval      sssub# = @@sub#
     C                   eval      ssfnd = @@fnd
     C                   eval      ssfyr = @@fyr
     C                   eval      sscase = @@case
    C*
    C* PROTECT THE SUBSET FIELDS FOR SUB#/FND/FYR/CASE
    C*
     C                   eval      *in50 = *on
    C*
    C* SET CASE PARMS FOR CALL TO ADJPGM WHEN SUBSETTED ON CASE
    C*
     C                   eval      #2sub# = sssub#
     C                   eval      #2fnd = ssfnd
     C                   eval      #2fyr = ssfyr
     C                   eval      #2case = sscase
X01  C                   else
     C                   eval      *in50 = *off
E01  C                   endif
    C*
    C* LOAD EOMB# AND PRE-CERT# INTO SUBSET FIELDS IF THE NUMBER OF
    C* PARMS BEING PASSED IS GREATER THAN 5.
    C*
B01  C                   if        q1prms > 5
    C*
    C* LOAD EOMB# INFORMATION INTO SSEOB# AND PROTECT IF NOT BLANK.
    C*
     C                   eval      sseob# = @@eob#
    C*
B02  C                   if        sseob# <> *zeros
     C                   eval      *in16 = *on
E02  C                   endif
    C*
    C* LOAD PRE-CERT INFORMATION INTO SSCERT AND PROTECT IF NOT BLANK.
    C*
B02  C                   if        q1prms > 6
    C*
     C                   movel(p)  @@cert        sscert
    C*
B03  C                   if        sscert <> *blanks
     C                   eval      *in17 = *on
E03  C                   endif
    C*
E02  C                   endif
    C*
E01  C                   endif
    C*
B01  C                   if        q1prms >= 8
     C                   move      @@sfdt        ssfrdt
     C                   move      @@stdt        sstodt
     C                   move      @@sfei        ssfein
     C                   move      @@smod        ssmod
    C*
B02  C                   if        ssfein = *zeros
     C*** Beg Chg *** 07/26/16 ********************************
     C************************     or ssmod = '000'
     C*****************  move      '   '         ssmod
     C                             or ssmod = *zeros
     C                   move      *zeros        ssmod
     C*** End Chg *** 07/26/16 ********************************
E02  C                   endif
    C*
B02  C                   if        ssfrdt <> *zeros
     C                             or sstodt <> *zeros
     C                   eval      *in18 = *on
E02  C                   endif
    C*
B02  C                   if        ssfein <> *zeros
     C*** Beg Chg *** 07/26/16 ********************************
     C*************************    or ssmod <> *blanks
     C                             or ssmod <> *zeros
     C*** End Chg *** 07/26/16 ********************************
E02  C                   endif
    C*
E01  C                   endif
     C*
     C* Check restriction flags and subset and protect.
     C*
     C                   movel(p)  q1user        kkusr
     C     upky          chain     pup01                              99
    C*
B01  C                   if        *in99=*off
    C*
B02  C                   select
    C*
S01  C                   when      upmrgr = 'Y'
     C                             and ##rcds = *off
     C                             and upsub# <> *zeros
     C                             and upfnd <> *zeros
     C                             or *in55 = *off
     C                             and upmrgr = 'Y'
     C                             and upsub# <> *zeros
     C                             and upfnd <> *zeros
     C                   eval      sssub#=upsub#
     C                   eval      ssfnd=upfnd
     C                   seton                                        5152
    C*
S02  C                   when      upmrsu = 'Y'
     C                             and ##rcds = *off
     C                             and upsub# <> *zeros
     C                             or *in55 = *off
     C                             and upmrsu = 'Y'
     C                             and upsub# <> *zeros
     C                   eval      sssub#=upsub#
     C                   seton                                        51
    C*
E02  C                   endsl
    C*
E01  C                   endif
    C*
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
    C*
     C     keyinz        begsr
    C*
    C*** GET SYSTEM INFO
    C*
     C                   call      'SA001R'
     C                   parm      *blank        ##wcif
     C                   parm      *blank        ##mcif
     C                   parm      *blank        ##wcin
     C                   parm      *blank        ##ar
     C                   parm      *blank        ##ntwk
     C                   parm      *blank        ##ur
     C                   parm      *blank        ##med
     C                   parm      *blank        ##edi
     C                   parm      *blank        ##wcdk
     C                   parm      *blank        ##rhab
     C                   parm      *blank        ##mss
     C                   parm      *blank        ##fsch
     C                   parm      *blank        ##prvb
     C                   parm      *blank        ##npwc
     C                   parm      *blank        ##os
     C                   parm      *blank        ##arls
     C                   parm      *blank        ##ccod
     C                   parm      *blank        ##crvw
     C                   parm      *blank        ##prom
     C                   parm      *blank        ##mcrv
     C                   parm      *blank        ##freq
     C                   parm      *blank        ##crac
     C                   parm      *blank        ##preo
     C                   parm      *blank        ##parn
     C                   parm      *blank        ##mgri
     C                   parm      *blank        ##kcif
     C                   parm      *blank        ##curs
     C                   parm      *blank        ##pcib
     C                   parm      *blank        ##clt#
     C*** Beg Add *** 09/28/15 *******************************
     C                   parm      *blank        ##ucpf
     C                   parm      *blank        ##dcf
     C                   parm      *blank        ##autb
     C                   parm      *blank        ##deif
     C                   parm      *blank        ##aure
     C                   parm      *blank        ##cred
     C                   parm      *blank        ##idrt
     C                   parm      *blank        ##cprv
     C                   parm      *blank        ##eft
     C                   parm      *blank        ##govr
     C                   parm      *blank        ##ncdc
     C                   parm      *blank        ##splt
     C                   parm      *blank        ##lst
     C                   parm      *blank        ##pblt
     C                   parm      *blank        ##inze
     C*** End Add *** 09/28/15 *******************************
    C*
     C                   eval      *in55 = *off
    C*
B01  C                   if        ##mgri = 'Y'
     C                   eval      *in55 = *on
E01  C                   endif
    C*
     C                   move      *loval        kklsn
     C                   move      *loval        kkfsn
     C                   move      *loval        kkeob#
     C                   move      *loval        kkfein
     C                   move      *loval        kkmod
     C                   move      *loval        kkss#
     C                   move      *loval        kkacdt
     C                   move      *loval        kkcntl
     C                   move      *loval        kkidoc
     C                   eval      kkfrdt = *hival
    C*
     C                   endsr
    C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
    C*
     C     keyset        begsr
    C*
    C* VIEW# 1 (Last Name and First Name)
    C*
B01  C                   if        *in21 = *on
     C                   movel(p)  scpst1        kklsn
     C                   movel(p)  scpsta        kkfsn
E01  C                   endif
    C*
B01  C                   if        scpstf <> *zeros
     C                             and *in21 = *on
    C*
B02  C                   if        *in21 = *on
     C                   move      scpstf        srdti
E02  C                   endif
    C*
B02  C                   if        *in21 = *on
     C                   call      'SRCVTDAT'    dtcymd
E02  C                   endif
    C*
B02  C                   if        *in21 = *on
     C                   movel     srdto         kkfrdt
E02  C                   endif
    C*
X01  C                   else
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkfrdt
E01  C                   endif
    C*
B01  C                   if        *in21 = *on
     C                   eval      kkeob# = *zeros
E01  C                   endif
    C*
    C* VIEW# 2 (Provider FEIN and MOD)
    C*
B01  C                   if        *in22 = *on
     C                   movel     scpst2        kkfein
     C                   move      scpstb        kkmod
E01  C                   endif
    C*
    C* VIEW# 3 (SS# and DOI)
    C*
B01  C                   if        *in23 = *on
     C                   movel     scpst3        kkss#
E01  C                   endif
    C*
B01  C                   if        *in23 = *on
     C                   movel     scpstc        srdti
     C                   clear                   srdto
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkacdt
E01  C                   endif
    C*
    C* VIEW# 4 (EOMB#)
    C*
B01  C                   if        *in24 = *on
     C                   move      scpst4        kkeob#
E01  C                   endif
    C*
    C*
    C* VIEW# 5 (Control Number)
    C*
B01  C                   if        *in25 = *on
     C                   movel(p)  scpst5        kkcntl
     C                   eval      kkeob# = 0
E01  C                   endif
    C*
    C* VIEW# 6 (Image Doc id)
    C*
B01  C                   if        *in26 = *on
     C                   movel(p)  scpst6        kkidoc
     C                   eval      kkeob# = 0
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
    C*
     C     keysfl        begsr
     C*** Beg Add *** 10/11/17 ******************************************
     C*** Beg Chg *** 11/15/17 ******************************************
    C**************     setoff                                       44
    C                   eval      errClmLck = '0'
     C*** End Chg *** 11/15/17 ******************************************
     C*** End Add *** 10/11/17 ******************************************
    C*
     C                   movel(p)  sclsn         kklsn
     C                   movel(p)  scfsn         kkfsn
     C                   movel(p)  hlpnam        kkpnam
     C                   movel     scfein        kkfein
     C                   movel     scmod         kkmod
     C                   movel(p)  scss#         kkss#
     C                   eval      kkacdt = hlsdoi
     C                   eval      kkeob# = sceob#
     C                   movel(p)  sccntl        kkcntl
     C                   movel(p)  scidoc        kkidoc
     C                   eval      kkfrdt = scfrd
    C*
     C                   endsr
    C*
    C*================================================================
    C* KJDO - F10 Search Subroutine
    C*================================================================
    C*
     C     kjdo          begsr
    C*
    C* Indicators for cursor positioning
    C*
     C                   eval      *in30 = *off
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off
     C                   eval      *in33 = *off
     C                   eval      *in34 = *off
     C                   eval      *in35 = *off
     C                   eval      *in36 = *off
     C                   eval      *in37 = *off
     C                   eval      *in38 = *off
     C***** Beg Dlt *** 02/27/20 *******************************************
    C*
    C* Subscriber Search
    C*
B01  C***                if        hfld = 'SSSUB#'
     C***                          and *in50=*off
     C***                          and *in51=*off
     C***                eval      hlsub# = sssub#
     C***                movel     sssmnm        hlsmnm
     C***                eval      ##rrn = 0
    C*
     C***                call      'F0055R'
     C***                parm      '2'           ##call
     C***                parm      ' '           ##kc
     C***                parm      ' '           ##kl
     C***                parm                    ##rrn
    C*
B02  C***                if        ##kc = '1'
     C***                          or ##kl = '1'
     C***                          or ##rrn = *zeros
     C***                eval      sssub# = hlsub#
     C***                movel     hlsmnm        sssmnm
     C***                eval      *in90 = *on
     C***                eval      *in30 = *on
X02  C***                else
    C*
     C***                call      'FA726R'
     C***                parm                    ##co#
     C***                parm                    ##sub#
     C***                parm                    ##name
     C***                parm                    ##rrn
     C***                parm      '1'           ##err
    C*
     C***                movel(p)  ##name        sssmnm
     C***                eval      sssub# = ##sub#
     C***                eval      *in90 = *on
     C***                eval      *in31 = *on
E02  C***                endif
    C*
     C***                exsr      datsbr
    C*
E01  C***                endif
     C***** End Dlt *** 02/27/20 *******************************************
     C***** Beg Dlt *** 02/27/20 *******************************************
    C*
    C* Group Search
    C*
B01  C***                if        hfld = 'SSFND'
     C***                          and *in50=*off
     C***                          and *in52=*off
     C***                eval      ##rrn = 0
     C***                eval      hlfnd = ssfnd
     C***                movel     ssfnnm        hlfnnm
    C*
     C***                call      'F0060R'
     C***                parm      '2'           ##call
     C***                parm      ' '           ##kc
     C***                parm      ' '           ##kl
     C***                parm                    ##rrn
    C*
B02  C***                if        ##kc = '1'
     C***                          or ##kl = '1'
     C***                          or ##rrn = *zeros
     C***                eval      ssfnd = hlfnd
     C***                movel     hlfnnm        ssfnnm
     C***                eval      *in90 = *on
     C***                eval      *in31 = *on
X02  C***                else
    C*
     C***                call      'FA727R'
     C***                parm                    ##co#
     C***                parm                    #2sub#
     C***                parm                    #2fnd
     C***                parm                    #2name
     C***                parm                    ##rrn
     C***                parm      '1'           #2err
    C*
     C***                eval      ssfnd = #2fnd
     C***                movel(p)  #2name        ssfnnm
     C***                eval      *in90 = *on
     C***                eval      *in32 = *on
E02  C***                endif
    C***
     C***                exsr      datsbr
    C*
E01  C***                endif
     C***** End Dlt *** 02/27/20 *******************************************
    C*
    C* Fund Year/Case Search
    C*
     C***** Beg Dlt *** 02/27/20 *******************************************
B01  C***                if        hfld = 'SSFYR'
     C***                          or hfld = 'SSCASE'
     C***** End Dlt *** 02/27/20 *******************************************
    C*
     C***** Beg Add *** 02/27/20 *******************************************
B01  C                   if        srchClm = *on
     C***** End Add *** 02/27/20 *******************************************
     C                   eval      hlfyr = ssfyr
     C                   eval      hlcase = sscase
    C*
     C                   call      'F0269R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##sub#
     C                   parm      *zeros        ##fnd
     C                   parm      *zeros        ##fyr
     C                   parm      *zeros        ##case
     C                   parm      *blanks       ##ss#
     C                   parm      *zeros        ##acdt
     C                   parm      ' '           ##rtcd
    C*
B02  C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fyr = *zeros
     C                             or ##case = *zeros
     C                   eval      ssfyr = hlfyr
     C                   eval      sscase = hlcase
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
X02  C                   else
     C                   eval      ssfyr = ##fyr
     C                   eval      sscase = ##case
     C                   eval      sssub# = ##sub#
     C                   eval      ssfnd = ##fnd
     C                   eval      kksub# = ##sub#
     C                   eval      kkfnd = ##fnd
     C                   eval      kkfyr = ##fyr
     C                   eval      kkcase = ##case
    C*
    C* get claimaint name
    C*
     C     cmky          chain     pcm01                              99
    C*
B03  C                   if        *in99 = *off
     C                   movel(p)  cmlsn         ##last
     C                   movel(p)  cmfsn         ##frst
     C                   move      '1'           ##term
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm                    ##term
     C                   movel(p)  ##last        ssname
E03  C                   endif
     C***** Beg Dlt *** 02/27/20 **************************************
    C*
    C* get subscriber name
    C*
    C*
     C***                call      'FA726R'
     C***                parm                    ##co#
     C***                parm      kksub#        ##sub#
     C***                parm                    ##name
     C***                parm                    ##rrn
     C***                parm      '0'           ##err
    C*
     C***                movel(p)  ##name        sssmnm
    C*
    C* get group name
    C*
    C*
     C***                call      'FA727R'
     C***                parm                    ##co#
     C***                parm      kksub#        #2sub#
     C***                parm      kkfnd         #2fnd
     C***                parm                    #2name
     C***                parm                    ##rrn
     C***                parm                    #2err
    C*
     C***                movel(p)  #2name        ssfnnm
     C***** End Dlt *** 02/27/20 *******************************************
    C*
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
E02  C                   endif
    C*
     C                   exsr      datsbr
    C*
E01  C                   endif
    C*
    C* Provider Search
    C*
     C***** Beg Dlt *** 02/27/20 *******************************************
B01  C***                if        hfld = 'SSFEIN'
     C***                          or hfld = 'SSMOD'
     C***** End Dlt *** 02/27/20 *******************************************
    C*
     C***** Beg Add *** 02/27/20 *******************************************
B01  C                   if        srchProv = *on
     C***** End Add *** 02/27/20 *******************************************
     C                   move      ssfein        hlfein
     C                   move      ssmod         hlmod
     C                   call      'F0032R'
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##fein
     C                   parm      *blanks       ##mod
     C                   parm      *blanks       ##rtcd
     C                   parm      *blanks       ##type
    C*
B02  C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fein = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
X02  C                   else
     C                   move      ##fein        ssfein
     C                   move      ##mod         ssmod
     C                   eval      *in90 = *on
     C                   eval      *in34 = *on
    C*
     C                   eval      kkfein = ssfein
     C                   movel     ssmod         kkmod
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
B03  C                   if        #1err <> '0'
     C                   movel(p)  #1name        sspdnm
E03  C                   endif
E02  C                   endif
    C*
     C                   exsr      datsbr
    C*
E01  C                   endif
    C*
     C***** Beg Dlt *** 02/27/20 *******************************************
B01  C***                if        hfld = 'SSBLST'
     C***                move      ssblst        hlblst
     C***                call      'F2380R'
     C***                parm      '2'           ##call
     C***                parm      *blanks       ##kc
     C***                parm      *blanks       ##kl
     C***                parm      *zeros        ##rrn
     C***                parm      'BLST'        ##tbid
    C*
B02  C***                if        ##kc = '1'
     C***                          or ##kl = '1'
     C***                          or ##rrn = *zeros
     C***                move      hlblst        ssblst
     C***                movel     ssbsds        hlbsds
     C***                eval      *in90 = *on
     C***                eval      *in35 = *on
X02  C***                else
     C***                movel     hlbsds        ssbsds
     C***                eval      *in90 = *on
     C***                eval      *in37 = *on
    C*
     C***                call      'FA728R'
     C***                parm                    ##co#
     C***                parm                    ##tbid
     C***                parm                    #1key
     C***                parm                    ##desc
     C***                parm                    ##rrn
     C***                parm      '1'           ##err
    C*
     C***                movel     #1key         ssblst
     C***                movel     ##desc        ssbsds
    C*
E02  C***                endif
    C*
     C***                exsr      datsbr
    C*
E01  C***                endif
    C*
B01  C***                if        hfld = 'SSBTYP'
    C*
     C***                call      'F2380R'
     C***                parm      '2'           ##call
     C***                parm      *blanks       ##kc
     C***                parm      *blanks       ##kl
     C***                parm      *zeros        ##rrn
     C***                parm      'BTYP'        ##tbid
    C*
B02  C***                if        ##kc = '1'
     C***                          or ##kl = '1'
     C***                          or ##rrn = *zeros
     C***                eval      *in90 = *on
     C***                eval      *in37 = *on
X02  C***                else
     C***                movel     hlbsds        ssbsds
     C***                eval      *in90 = *on
     C***                eval      *in36 = *on
    C*
     C***                call      'FA728R'
     C***                parm                    ##co#
     C***                parm                    ##tbid
     C***                parm      *blanks       #1key
     C***                parm      *blanks       ##desc
     C***                parm                    ##rrn
     C***                parm      '1'           ##err
    C*
     C***                movel     #1key         ssbtyp
     C***                movel     ##desc        scbtds
    C*
E02  C***                endif
    C*
     C***                exsr      datsbr
    C*
E01  C***                endif
     C***** End Dlt *** 02/27/20 *******************************************
    C*
    C* Initial User Search
    C*
     C***** Beg Dlt *** 02/27/20 *******************************************
B01  C***                if        hfld = 'SSIUSR'
     C***** End Dlt *** 02/27/20 *******************************************
    C*
     C***** Beg Add *** 02/27/20 *******************************************
B01  C                   if        srchIUsr = *on
     C***** End Add *** 02/27/20 *******************************************
     C                   movel(p)  ssiusr        hliusr
     C                   movel(p)  ssusnm        hlusnm
     C                   call      'S0005R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm      *zeros        ##view
B02  C                   if        ##rrn = *zeros
     C                   movel(p)  hliusr        ssiusr
     C                   movel(p)  hlusnm        ssusnm
     C                   eval      *in90 = *on
     C                   eval      *in36 = *on
X02  C                   else
    C*
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      ##rrn         ##rrn
     C                   parm      *blanks       ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
B03  C                   if        ##rtcd = *blanks
     C                   movel(p)  ##user        ssiusr
     C                   movel(p)  ##usnm        ssusnm
X03  C                   else
     C                   clear                   ssiusr
     C                   clear                   ssusnm
E03  C                   endif
    C*
     C                   eval      *in90 = *on
E02  C                   endif
    C*
     C                   exsr      datsbr
    C*
E01  C                   endif
    C*
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*
    C*================================================================
    C* KNDO - DARTS Subroutine
    C*================================================================
    C*
     C     kndo          begsr
    C*
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT6 - Detail Charge List
    C*================================================================
    C*
     C     opt6          begsr
    C*
     C                   move      scacdt        x1dt
     C                   move      x1dtm         x2dtm
     C                   move      x1dtd         x2dtd
     C                   move      x1dty         x2dty
     C                   move      x2dt          srdti
    C*
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         qqacdt
     C                   move      scmod         qqmod
    C*
     C*** Beg Chg *** 05/03/19 *********************************************
     C*****************
     C*****************  call      'F9951R'
     C*****************  parm                    scprds
     C*****************  parm      ' '           ##call
     C*****************  parm                    scsub#
     C*****************  parm                    scfnd
     C*****************  parm                    scss#
     C*****************  parm                    qqacdt
     C*****************  parm                    scfein
     C*****************  parm                    qqmod
     C*****************  parm      'Y'           qqmode
      /Free
                         Callp DtlChgsList(scFnd:scFyr:scCase:scFein:scMod);
      /End-Free
     C*** End Chg *** 05/03/19 *********************************************
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT7 - Work with Detail
    C*================================================================
    C*
     C     opt7          begsr
    C*
     C                   move      'D'           ##mode
       //*** Beg Add *** 03/19/24 *************************************
       // If the Adj State is not an installed state (FTABLxx not found)
       // then dont call the override routines and pass blank to FA730R
       inAdjState = scAst;
       Chain(n)scDbrr pfrcdf;
       strCmd = 'CHKOBJ OBJ(' + 'FTABL' + %TRIM(EOAST) + ') ' +
       'OBJTYPE(*FILE)';
       Monitor;
       RunCmd(%Trim(strCmd):%Len(%Trim(strCmd)));
       On-error;
          isOvr = 'N';
          inAdjState = ' ';
       Endmon;
       //*** End Add *** 03/19/24 *************************************
    C*
       //*** Beg Chg *** 03/19/24 *************************************
     C**************     exsr      ovride
       If isOvr <> 'N';
          Exsr Ovride;
       Endif;
       //*** End Chg *** 03/19/24 *************************************
     C                   move      ' '           ##kl
    C*
     C                   call      'FA730R'
     C*** Beg Chg *** 03/19/24 *****************************************
     C*****************  parm      scast         ##ast
     C                   parm      inAdjState    ##ast
     C*** End Chg *** 03/19/24 *****************************************
     C                   parm      scsys         ##sys
     C                   parm      scbtyp        ##btyp
     C                   parm                    ##depg
    C*
     C                   move(p)   ##depg        ##pgm
    C*
     C                   clear                   ##pgm2
    C*
     C                   call      'F0390R'
     C                   parm                    ##mode
     C                   parm                    ##kl
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##pgm
     C                   parm                    ##pgm2
    C*
       //*** Beg Chg *** 03/19/24 *************************************
     C*****************  exsr      dlovrd
       If isOvr <> 'N';
          Exsr Dlovrd;
       Endif;
       //*** End Chg *** 03/19/24 *************************************
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT8 - Work with Amounts
    C*================================================================
    C*
     C     opt8          begsr
    C*
     C                   call      'F0396R'
     C                   parm      ' '           ##call
     C                   parm                    scdbrr
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT10 - Work with Bill Status History
    C*================================================================
    C*
     C     opt10         begsr
    C*
     C                   exsr      ovride
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   move      sceob#        kkeob#
    C*
     C     esky          chain     pes01                              99
    C*
B01  C                   if        *in99 = *off
     C                   eval      ##rrn = dbrrna
E01  C                   endif
    C*
    C***** Beg Chg *** 05/22/19 ***************************
     C/free

        // call Provider Bills Detail program
        inEOrrn = %EditC(scdbrr:'X');
        inMode  = *blanks;

        callp PBDetails(inEOrrn:inMode);

      /end-free
     C*
     C****               call      'F0395R'
     C****               parm      ' '           ##call
     C****               parm      ' '           ##kc
     C****               parm      ' '           ##kl
     C****               parm                    ##rrn
     C****               parm      scdbrr        ##rrn2
    C***** End Chg *** 05/22/19 ***************************
    C*
     C                   exsr      dlovrd
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT11 - Work with EOB Notes
    C*================================================================
    C*
     C     opt11         begsr
    C*
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   move      sceob#        kkeob#
    C*
     C     esky          chain     pes01                              99
    C*
B01  C                   if        *in99 = *off
     C                   eval      ##rrn = dbrrna
E01  C                   endif
    C*
     C                   call      'F0355R'
     C                   parm      '1'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm      scdbrr        ##rrn2
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT12 - Print EOMB
    C*================================================================
    C*
     C     opt12         begsr
    C*
     C                   move      scdbrr        #1rrn
    C*
     C                   call      'SA012R'
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##outq
     C                   parm      *blanks       ##rtcd
    C*
B01  C                   if        ##rtcd = *blanks
     C                   movel(p)  ##outq        ##que
E01  C                   endif
    C*
     C                   call      'F0340C'
     C                   parm                    #1rrn
     C                   parm                    scast
     C                   parm      'P'           #1mode
     C                   parm                    ##que
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT13 - Display EOMB
    C*================================================================
    C*
     C     opt13         begsr
    C*
     C                   clear                   ##que
     C                   move      scdbrr        #1rrn
     C                   call      'F0340C'
     C                   parm                    #1rrn
     C                   parm                    scast
     C                   parm      'D'           #1mode
     C                   parm                    ##que
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT14 - DARTS
    C*================================================================
    C*
     C     opt14         begsr
    C*
     C                   move(p)   ##co#         #1co#
     C                   move(p)   scsub#        #1sub#
     C                   move(p)   scfnd         #1fnd
     C                   move(p)   scfyr         #1fyr
     C                   move(p)   sccase        #1case
    C*
     C                   call      'FA703R'      drtspl
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT15 - Pre-Cert Info
    C*================================================================
    C*
     C     opt15         begsr
    C*
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   movel     sccert        kkcert
     C                   eval      kkext = scext
    C*
     C                   call      'FA732R'
     C                   parm      ##co#         ##co#
     C                   parm      kksub#        ##sub#
     C                   parm      kkfnd         ##fnd
     C                   parm      kkfyr         ##fyr
     C                   parm      kkcase        ##case
     C                   parm      kkcert        ##cert
     C                   parm      kkext         #1ext
     C                   parm                    ##rrn
    C*
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
    C*
     C                   call      'U0060R01'
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm      'D'           ##mode
     C                   parm                    ##rrn
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    ##cert
     C                   parm                    ##ext
     C                   parm      ' '           ##ocst
     C                   parm      'Y'           ##cont
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT16 - History
    C*================================================================
    C*
     C     opt16         begsr
    C*
    C*  Call new history pgm F0391R
    C*
     C                   call      'F0391R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    scfyr
     C                   parm                    sccase
     C                   parm                    sceob#
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT20 - Pended items
    C*================================================================
    C*
     C     opt20         begsr
    C*
    C*  Call Pended items pgm F0398R
    C*
     C                   call      'F0398R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    scfyr
     C                   parm                    sccase
     C                   parm                    sceob#
     C                   endsr
    C*================================================================
    C* OPT18 - Option 18 (EDI LOG HISTORY)
    C*================================================================
    C*
     C     opt18         begsr
    C*
     C                   move      scsub#        ##sub#
     C                   move      scfnd         ##fnd
     C                   move      scfyr         ##fyr
     C                   move      sccase        ##case
     C                   move      scss#         ##ss#
     C                   move      shacdt        ##acdt
     C                   move      sceob#        ##eob#
    C*
     C                   call      'F0875R'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      @@call        ##call
     C                   parm                    ##mode
     C                   parm                    ##co#
     C                   parm                    ##sub#
     C                   parm                    ##fnd
     C                   parm                    ##fyr
     C                   parm                    ##case
     C                   parm                    ##ss#
     C                   parm                    ##acdt
     C                   parm                    ##eob#
     C                   parm      '837'         ##tset
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT19 - Option 19 (Call EDI Log)
    C*================================================================
    C*
     C     opt19         begsr
    C*
     C                   move      ##co#         qqco#
     C                   move      scsub#        qqsub#
     C                   move      scfnd         qqfnd
     C                   move      scfyr         qqfyr
     C                   move      sccase        qqcase
     C                   move      sceob#        qqeob#
     C                   call      'F6250R'
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqeob#
     C                   parm                    qqrcn
     C                   parm      'C'           qqactf
     C                   parm      ' '           qqrtcd
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT21 - Check info
    C*================================================================
    C*
     C     opt21         begsr
    C*
     C                   move      ##co#         #1co#
     C                   move      scfnd         #1fnd
     C                   move      scsub#        #1sub#
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
     C                   move      sceob#        #1eob#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
     C                   call      'W0070R50'                           99
     C                   parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    #1co#
     C                   parm      #1wcf#        #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      @@call        @@call
     C                   parm                    #1eob#
    C*
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT22 - View Prov Info
    C*================================================================
    C*
     C     opt22         begsr
    C*
     C                   movel     scfein        zzfein
     C                   movel     scmod         zzmod
    C*
     C                   call      'F0032R'
     C                   parm      '1'           ##call
     C                   parm      zzfein        ##fein
     C                   parm      zzmod         ##mod
     C                   parm                    ##rtcd
     C                   parm                    ##type
     C                   parm                    ##1n4
     C                   parm      'Y'           ##1prv
    C*
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT30 - Letters
    C*================================================================
    C*
     C     opt30         begsr
    C*
     C                   call      'F0399R'
     C                   parm      @@call        ##call
     C     *inkc         parm      *off          ##kc
     C     *inkl         parm      *off          ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      scdbrr        ##rrn2
     C                   parm      *zeros        ##rrn3
    C*
     C                   endsr
    C*
    C*** BEG ADD *** 08/10/12 ****************************************
    C*================================================================
    C* OPT40 - PRINT IMAGE DOC ID ON EOMB
    C*================================================================
    C*
     C     opt40         begsr
    C*
     C                   call      'FA780R'
     C     *inkc         parm      *off          ##kc
     C     *inkl         parm      *off          ##kl
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##mode
    C*
     C                   endsr
    C*
    C*** END ADD *** 08/10/12 ****************************************
    C*
    C*================================================================
    C* OPT84 - Diary Adjuster
    C*================================================================
    C*
     C     opt84         begsr
    C*
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
    C*
B01  C                   if        *in99=*off
    C*
     C                   movel(p)  cmclad        kkstaf
     C*
     C     upky2         chain     pup02                              99
    C*
B02  C                   if        *in99=*off
     C*
     C                   movel(p)  upusr         $$dusr
     C                   move      cmco#         #1co#
     C                   move      cmsub#        #1sub#
     C                   move      cmfnd         #1fnd
     C                   move      cmfyr         #1fyr
     C                   move      cmcase        #1case
     C                   move      sceob#        xxeob#            9
     C*
     C                   call      'FA703R'
     C                   parm      'K'           $$RTCD            1
     C                   parm      ##wcin        $$WCIN            1
     C                   parm      ##wcdk        $$WCDK            1
     C                   parm                    #1CO#             3
     C                   parm                    #1SUB#            5
     C                   parm                    #1FND             3
     C                   parm                    #1FYR             3
     C                   parm                    #1CASE            7
     C                   parm      *blanks       $$KEY            40
     C                   parm      *blanks       $$PGM            10
     C*
     C                   time                    dsdttm
     C*
     C                   move      dsdate        $$date
     C                   eval      $$fold=#1co#+#1sub#+#1fnd+#1fyr+#1case+xxeob#
     C*
     C                   call      'SC001R'
     C                   parm      optnm2        $$prod           40
     C                   parm      *blanks       $$call            1
     C                   parm                    $$key            40
     C                   parm                    $$pgm            10
     C                   parm                    $$fold           80
     C                   parm      'F0350C'      $$doc            12
     C                   parm                    $$date            8
     C                   parm      'MCADJ'       $$rmcd            5
     C                   parm      *blanks       $$drmk           60
     C                   parm                    $$dusr           10
     C                   parm      'Y'           $$rfld            1
     C                   parm      'Y'           $$rdoc            1
     C                   parm      *blanks       $$rdat            1
     C                   parm      'Y'           $$rrcd            1
     C                   parm      *BLANKS       $$rcmt            1
     C                   parm      *BLANKS       $$rarm            1
     C                   parm      'Y'           $$rdtp            1
     C                   parm      'Y'           $$rdus            1
     C*
X02  C                   else
     C                   seton                                        8990
E02  C                   endif
    C*
X01  C                   else
     C                   seton                                        8990
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT81 - Claim info
    C*================================================================
    C*
     C     opt81         begsr
    C*
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
    C*
     C                   call      'FA701R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       ##outs
    C*
B01  C                   if        ##rtcd = *blank
     C                             and ##outs <> 'Y'
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
B02  C                   if        ##rtcd = *blank
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
    C*
     C                   call      'WA004R'
     C                   parm                    #1co#
     C                   parm      #1wcf#        #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      *zeros        #2rrn
    C*
B03  C                   if        #2rrn <> 0
    C*
     C                   call      'W0070R'
     C                   parm      '1'           ##call
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'M'           ##mode
     C                   parm                    #2rrn
     C                   parm      *blanks       ##sort
X03  C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
E03  C                   endif
    C*
X02  C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
E02  C                   endif
    C*
X01  C                   else
     C                   eval      *in85 = *on
     C                   eval      *in90 = *on
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT82 - Case info
    C*================================================================
    C*
     C     opt82         begsr
    C*
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
    C*
B01  C                   if        *in99 = *off
    C*
     C                   call      'F0260R'
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'L'           ##mode
     C                   parm      dbrcm1        ##rrn
     C                   parm      *blanks       ##ss#
     C                   parm      *zeros        ##acdt
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* OPT99 - Soft Delete of EOMB
    C*================================================================
    C*
     C     opt99         begsr
    C*
     C                   eval      scsf1r = sfrrn1
     C                   add       1             sfrrn2               14
    C*
     C                   write     scr02s
    C*
     C                   eval      *in08 = *on
     C                   eval      scrrn2 = 1
    C*
     C                   write     scr02
     C                   exfmt     scr02c
    C*
B01  C                   if        *inkl = *off
     C                   move      'G'           ##mode
    C*
    C*** CALL BILL STATUS HISTORY
    C*
     C                   eval      ##rrn = 0
     C                   eval      ##rrn2 = scdbrr
     C                   clear                   ##stcd
     C                   clear                   ##reas
     C                   movel(p)  q1pgm         #1pgm
    C*
     C                   call      'F0395R01'
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm      'A'           #1mode
     C                   parm                    ##rrn
     C                   parm                    ##rrn2
     C                   parm      'D'           ##blst
     C                   parm                    ##stcd
     C                   parm                    ##reas
     C                   parm                    #1pgm
    C*

B02  C                   if        ##kl = *off
     C                             and ##kc = *off
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
         Eoss# = UnMaskData('SSN':'Eoss#':scss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   call      acdpgm        acdpl
E02  C                   endif
    C*
E01  C                   endif
    C*
     C                   eval      *in14 = *off
    C*
     C                   endsr
    C*
    C*================================================================
    C* OVRIDE - Overide files based  on adjudication state
    C*================================================================
    C*
     C     ovride        begsr
    C*
     C***  Begin Chg  ***  06/27/12  **************************************
     C**********         eval      length = 56
     C                   eval      length = 60
     C***  End   Chg  ***  06/27/12  **************************************
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  scast         qqst
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(2)        qqcmd
     C                   movel(p)  scast         qqst
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(3)        qqcmd
     C                   movel(p)  scast         qqst
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(4)        qqcmd
     C                   movel(p)  scast         qqst
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(5)        qqcmd2
     C                   movel(p)  scast         qqst2
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd2
     C                   parm                    length
     C                   movel(p)  qca(6)        qqcmd2
     C                   movel(p)  scast         qqst2
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd2
     C                   parm                    length
     C                   movel(p)  qca(7)        qqcmd
     C                   movel(p)  scast         qqst
    C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
    C*
     C                   endsr
    C*
    C*================================================================
    C* MVDB04 - Move data base fields to SCR04
    C*================================================================
    C*
     C     mvdb04        begsr
    C*
    C** Get standard screen headings
    C*
    C**** Beg Chg *** 02/03/17 ****************************************
     C**** scdbrr        chain     pfrcdf                             99
     C     scdbrr        chain(n)  pfrcdf                             99
    C**** End Chg *** 02/03/17 ****************************************
    C*
     C                   movel     eospcd        scspcd
     C                   movel     eontwk        scnwcd
     C                   movel     eoflic        scsvzp
     C*
     C                   setoff                                       69
B01  C                   if        scnwcd <> *blanks
     C                   seton                                        69
E01  C                   endif
    C*
     C                   movel     *hival        kkidt
     C                   movel     *hival        kkitim
    C*
     C     esky2         setgt     pes01
     C     esky          readpe    pes01                                  99
    C*
B01  C                   if        *in99 = *off
     C                   eval      screas = esreas
X01  C                   else
     C                   eval      screas = *blanks
E01  C                   endif
    C*
     C                   move      eoco#         #1co#
     C                   move      eosub#        #1sub#
     C                   move      eofnd         #1fnd
     C                   move      eofyr         #1fyr
     C                   move      eocase        #1case
     C                   clear                   ##rtcd
     C                   clear                   #1l1
     C                   clear                   #1l2
    C*
     C                   call      'FA702R'
     C                   parm                    ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    #1l1
     C                   parm                    #1l2
    C*
     C                   movel     #1l1          schd1
     C                   movel     #1l2          schd2
    C*
     C                   clear                   ##rtcd
     C                   clear                   #1l1
    C*
     C                   call      'FA707R'
     C                   parm                    ##rtcd
     C                   parm      scdbrr        @@rrn2
     C                   parm                    #1l1
    C*
     C                   movel     #1l1          schd3
    C*
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
    C*
     C     mvdbsc        begsr
    C*
B01  C                   if        vldopt = 'N'
    C*
     C                   exsr      assrrn
    C*
    C*
E01  C                   endif
    C*
     C                   movel     eocrpe        sccrpe
     C                   movel     eocntl        sccntl
     C                   movel     eoidoc        scidoc
     C                   eval      scfrd = eofrdt
     C                   movel     eoast         scast
     C                   movel     eosys         scsys
     C                   movel     eobtyp        scbtyp
     C                   movel(p)  eolsn         sclsn
     C                   movel(p)  eofsn         scfsn
     C                   movel(p)  sclsn         ##last
     C                   movel(p)  scfsn         ##frst
     C                   move      '1'           ##term
    C*
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm                    ##term
     C                   movel     ##last        scname
    C*
     C                   move      eoeob#        sceob#
     C                   eval      scrcn = eorcn
    C*
    C* Display reconsiderations in red.
    C*
     C                   move      *off          *in68
    C*
B01  C                   if        eorcn > 0
     C                   move      *on           *in68
E01  C                   endif
    C*
     C                   movel(p)  eoeob#        qqnum
     C                   clear                   qqmask
    C*
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         aleob#
    C*
     C                   eval      kkfein = eofein
     C                   eval      kkmod = eomod
    C*
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
B01  C                   if        #1err <> '0'
     C                   movel(p)  #1name        scpnam
E01  C                   endif
    C*
    C** 'HLPNAM' FIELD IS USED TO HOLD ENTIRE PROVIDER NAME FOR
    C**  USE IN KKPNAM KEY FIELD FOR PAGING (KEYSFL SUBROUTINE)
    C*
     C                   movel     #1name        hlpnam
    C*
     C                   eval      srdti = eofrdt
    C*
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
    C*
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         scfrdt
    C*
     C                   eval      srdti = eotodt
    C*
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
    C*
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         sctodt
    C*
     C                   eval      srdti = eoacdt
     C                   eval      hlsdoi = eoacdt
    C*
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel     srdto         shacdt
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
    C*
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         scacdt
    C*
B01  C                   if        eoblst = 'D'
     C                   movel(p)  'DEL'         scblst
X01  C                   else
     C                   movel(p)  eoblst        scblst
E01  C                   endif
    C*
     C                   movel(p)  eoidoc        scidoc
    C*
     C                   move      eofein        scfein
     C                   move      scfein        alfein
     C                   move      eomod         scmod
     C                   move      scmod         almod
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
         scss# = MaskData('SSN':'eoss#':eoss#);
      /End-free
     C*****              movel(p)  eoss#         scss#
      ***** End Chg *** 06/09/20 *******************************************
    C*
     C                   movel(p)  eochrg        qqnum
     C                   movel(p)  'X,XXX.XX'    qqmask
     C                   call      'SA020R'
     C                   parm                    qqnum
     C                   parm                    qqmask
    C*
     C                   movel     qqnum         scchrg
     C                   move      scchrg        alchrg
    C*
     C                   movel(p)  eopaid        qqnum
     C                   movel(p)  'X,XXX.XX'    qqmask
    C*
     C                   call      'SA020R'
     C                   parm                    qqnum
     C                   parm                    qqmask
    C*
     C                   movel     qqnum         scpaid
     C                   move      scpaid        alpaid
    C*
     C                   eval      scsub# = eosub#
     C                   eval      scfnd = eofnd
     C                   eval      scfyr = eofyr
     C                   eval      sccase = eocase
    C*
     C                   clear                   scdata
    C*
B01  C                   select
    C*
    C*** LOAD VIEW# 1 FIELDS
    C*
S01  C                   when      hd# = 1
     C                             or hd# = 7
     C                             or hd# = 13
     C                             or hd# = 18
     C                   eval      y = 1
     C                   movea     scname        dta1(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta1(y)
     C                   eval      y = y + 11
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta1(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta1(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta1(y)
     C                   movea(p)  dta1          scdata
    C*
    C*** LOAD VIEW# 2 FIELDS
    C*
S02  C                   when      hd# = 2
     C                             or hd# = 8
     C                             or hd# = 14
     C                             or hd# = 20
     C                   eval      y = 1
     C                   movea     alfein        dta2(y)
     C                   eval      y = y + 10
     C                   movea     almod         dta2(y)
     C                   eval      y = y + 5
     C                   movea     scname        dta2(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta2(y)
     C                   movea(p)  dta2          scdata
    C*
    C*** LOAD VIEW# 3 FIELDS
    C*
S03  C                   when      hd# = 3
     C                             or hd# = 9
     C                             or hd# = 15
     C                             or hd# = 21
     C                   eval      y = 1
     C                   movea     scss#         dta3(y)
     C                   eval      y = y + 11
     C                   movea     scacdt        dta3(y)
     C                   eval      y = y + 11
     C                   movea     aleob#        dta3(y)
     C                   eval      y = y + 10
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta3(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta3(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta3(y)
     C                   movea(p)  dta3          scdata
    C*
    C*** LOAD VIEW# 4 FIELDS
    C*
S04  C                   when      hd# = 4
     C                             or hd# = 10
     C                             or hd# = 16
     C                             or hd# = 22
     C                   eval      y = 1
     C                   movea     aleob#        dta4(y)
     C                   eval      y = y + 11
     C                   movea     scname        dta4(y)
     C                   eval      y = y + 21
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta4(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta4(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta4(y)
     C                   movea(p)  dta4          scdata
    C*
    C*** LOAD VIEW# 5 FIELDS
    C*
S05  C                   when      hd# = 5
     C                             or hd# = 11
     C                             or hd# = 17
     C                             or hd# = 23
     C                   eval      y = 1
     C                   movea     eocntl        dta5(y)
     C                   eval      y = y + 15
     C                   movea     scname        dta5(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta5(y)
     C                   movea(p)  dta5          scdata
    C*
    C*** LOAD VIEW# 6 FIELDS
    C*
S06  C                   when      hd# = 6
     C                             or hd# = 12
     C                             or hd# = 18
     C                             or hd# = 24
     C                   eval      y = 1
     C                   movea     eoidoc        dta6(y)
     C                   eval      y = y + 15
     C                   movea     scname        dta6(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta6(y)
     C                   movea(p)  dta6          scdata
    C*
E01  C                   endsl
    C*
     C*** Beg Add ***** 11/22/16 ***************************
    C* Load Bill Import Error Check box

     C                   Eval      scBErr = EOberr
     C*** End Add ***** 11/22/16 ***************************
B01  C                   select
    C*
    C* LOAD INFO #1
    C*
S01  C                   when      info# = 1
     C                   eval      x = 2
     C                   movea     scfrdt        inf1(x)
     C                   eval      x = x + 12
     C                   movea     sctodt        inf1(x)
     C                   eval      x = x + 13
     C                   movea     scblst        inf1(x)
     C                   movea     inf1          hlinf1
     C                   move      hlinf1        scdata
    C*
    C* LOAD INFO #2
    C*
S02  C                   when      info# = 2
     C                   eval      x = 1
     C                   movea     alchrg        inf2(x)
     C                   eval      x = 12
     C                   movea     alpaid        inf2(x)
     C                   eval      x = x + 12
     C                   movea     scbtyp        inf2(x)
     C                   eval      x = x + 3
     C                   movea     eoast         inf2(x)
     C                   movea     inf2          hlinf2
     C                   move      hlinf2        scdata
    C*
    C* LOAD INFO #3
    C*
S03  C                   when      info# = 3
     C                   eval      x = 2
     C                   movea     eocntl        inf3(x)
     C                   eval      x = x + 20
     C                   movea     scblst        inf3(x)
     C                   movea     inf3          hlinf3
     C                   move      hlinf3        scdata
    C*
    C* LOAD INFO #4
    C*
S04  C                   when      info# = 4
     C                   eval      x = 2
     C                   movea     eopnam        inf4(x)
     C                   eval      x = x + 24
     C                   movea     ' '           inf4(x)
     C                   eval      x = x + 1
     C                   movea     scblst        inf4(x)
     C                   movea     inf4          hlinf4
     C                   move      hlinf4        scdata
    C*
E01  C                   endsl
    C*
B01  C                   if        info# = 1
     C                   movel     heada         schdla
X01  C                   else
     C                   clear                   schdla
E01  C                   endif
    C*
    C* THE FOLLOWING ARE HIDDEN FIELDS THAT ARE NOT DISPLAYED BUT
    C* USED IN THIS PROGARM
    C*
     C                   movel(p)  eocert        sccert
     C                   eval      scext = eoext
    C*
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
    C*
     C     othfks        begsr
    C*
     C                   eval      *in70 = *off
    C*
     C     *inkj         caseq     *on           kjdo
     C***** Beg Add *** 02/27/20 **************************************
     C     srchClm       caseq     *on           kjdo
     C     srchProv      caseq     *on           kjdo
     C     srchIUsr      caseq     *on           kjdo
     C***** End Add *** 02/27/20 **************************************
     C     *inkn         caseq     *on           kndo
     C                   endcs
    C*
     C                   endsr
    C*
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
    C*
     C     othopt        begsr
    C*
B01  C                   if        *in55 = *on
    C*
     C                   move      @@call        svcall
     C                   move      *in09         svin09
    C*
B02  C                   if        scauth = 'I'
     C                   move      *on           *in09
     C                   move      '1'           @@call
     C                   move      '1'           ##call
E02  C                   endif
    C*
E01  C                   endif
    C*
     C     scsel         caseq     '6'           opt6
     C     scsel         caseq     '7'           opt7
     C     scsel         caseq     '8'           opt8
     C     scsel         caseq     '10'          opt10
     C     scsel         caseq     '11'          opt11
     C     scsel         caseq     '12'          opt12
     C     scsel         caseq     '13'          opt13
     C     scsel         caseq     '14'          opt14
     C     scsel         caseq     '15'          opt15
     C     scsel         caseq     '21'          opt21
     C     scsel         caseq     '22'          opt22
     C     scsel         caseq     '16'          opt16
     C     scsel         caseq     '18'          opt18
     C     scsel         caseq     '19'          opt19
     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '30'          opt30
     C     scsel         caseq     '40'          opt40
     C   20scsel         caseq     '84'          opt84
     C     scsel         caseq     '81'          opt81
     C     scsel         caseq     '82'          opt82
     C     scsel         caseq     '99'          opt99
     C                   endcs
    C*
B01  C                   if        *in55 = *on
     C                   move      svcall        @@call
     C                   move      svin09        *in09
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
    C*
     C     pgmend        begsr
    C*
     C*** Begin Add *** 04/30/15 ***************************************
     C                   If        rstPgm = *Off
     C*** End   Add *** 04/30/15 ***************************************
     C                   close     faeolh
     C                   close     faeoli
     C                   close     faeolj
     C                   close     faeolk
     C                   close     faeol32
     C                   close     faeol75
     C*** Begin Add *** 04/30/15 ***************************************
     C                   EndIf
     C*** End   Add *** 04/30/15 ***************************************
    C*
B01  C                   if        qqflag = '0'
    C*
    C* Call CL to release the lock on Data area FDAILYA
    C*
     C                   call      'F0350C03'
E01  C                   endif
    C*
B01  C                   if        stop = *on
     C                   move      *off          stop
E01  C                   endif
    C*
B01  C                   if        WMXRLopen = *on
     C                   close     WMXRL
     C                   eval      WMXRLopen = *off
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
    C*
     C     pgmstr        begsr
    C*
     C                   eval      srcwdw=*off
    C*
    C* Call CL to place a lock on Data area FDAILYA
    C*
     C                   call      'F0350C02'
     C                   parm      '0'           qqflag
    C*
B01  C                   if        qqflag = '1'
     C                   move      *on           *inlr
     C                   return
E01  C                   endif
    C*
     C                   eval      *in12 = *on
     C                   move      *off          stop
    C*
     C                   movel(p)  q1user        scuser
    C*
     C     scuser        chain(n)  pup01                              99
    C*
    C* Check for active user authorization
    C*
B01  C                   if        *in55 = *on
     C                   call      'SA008R'      rstpl                  99
     C                   move      upmrsu        ##mrsu
     C                   move      upmrgr        ##mrgr
     C                   move      upmrem        ##mrem
    C*
B02  C                   if        ##rcds = *on
     C                             and ##mrsu = 'Y'
     C                   exsr      getsub
E02  C                   endif
    C*
E01  C                   endif
    C*
     C                   open      faeolh
     C                   open      faeoli
     C                   open      faeolj
     C                   open      faeolk
     C                   open      faeol32
     C                   open      faeol75
    C*
    C* CALL API FOR SYSTEM FLAGS
    C*
     C                   call      'SA001R'
     C                   parm      ' '           ##wcif
     C                   parm      ' '           ##mcif
     C                   parm      ' '           ##wcin
     C                   parm      ' '           ##ar
     C                   parm      ' '           ##ntwk
     C                   parm      ' '           ##ur
     C                   parm      ' '           ##med
     C                   parm      ' '           ##edi
     C                   parm      ' '           ##wcdk
     C                   parm      ' '           ##rhab
     C                   parm                    ##mss
     C                   parm                    ##fsch
     C                   parm                    ##prvb
     C                   parm                    ##npwc
    C*
    C* Determine if OPT15=Pre-Cert Info should be displayed
    C*
B01  C                   if        ##ur = 'Y'
     C                   movel     pciopt        ofd(15)
X01  C                   else
     C                   movel     clropt        ofd(15)
E01  C                   endif
    C*
    C* Determine if OPT16=EDT Log Info and OPT17=Send Subsq 837
    C* should be displayed
    C*
B01  C                   if        ##edi = 'Y'
     C                   movel     ediopt        ofd(18)
     C                   movel     ssqopt        ofd(19)
X01  C                   else
     C                   movel     clropt        ofd(18)
     C                   movel     clropt        ofd(19)
E01  C                   endif
    C*
    C* Determine if OPT21=Check info should be displayed
    C*
B01  C                   if        ##wcin = 'Y'
     C                             and ##npwc <> 'Y'
     C                   movel     vckopt        ofd(21)
X01  C                   else
     C                   movel     clropt        ofd(21)
E01  C                   endif
    C*
    C* Determine if OPT81=Claim Info should be displayed
    C*
B01  C                   if        ##wcif = 'Y'
     C                             and ##npwc <> 'Y'
    C*** BEG CHG *** 08/10/12 ****************************************
     C*****              movel     clmopt        ofd(24)
     C                   movel     clmopt        ofd(25)
    C*** END CHG *** 08/10/12 ****************************************
X01  C                   else
    C*** BEG CHG *** 08/10/12 ****************************************
     C*****              movel     clmopt        ofd(24)
     C                   movel     clropt        ofd(25)
    C*** END CHG *** 08/10/12 ****************************************
E01  C                   endif
    C*
B01  C                   if        ##wcif = 'Y'
     C                   eval      WMXRLopen = *on
     C                   open      WMXRL
E01  C                   end
    C*
    C** GET INITIAL VIEW FROM CONTROL FILE SFBPF (THEIR ARE 4 POSSIBLE VIEWS)
    C*
     C                   call      'SA009R'
     C                   parm      *zeros        ##eobd
     C                   parm      ' '           ##eobi
     C                   parm      ' '           ##arin
     C                   parm      ' '           ##acis
     C                   parm      ' '           ##pay0
     C                   parm      ' '           ##sdts
     C                   parm      ' '           ##mciv
     C                   parm      ' '           ##pbiv
    C*
B01  C                   if        ##pbiv = '1'
     C                             or ##pbiv = '2'
     C                             or ##pbiv = '3'
     C                             or ##pbiv = '4'
     C                             or ##pbiv = '5'
     C                             or ##pbiv = '6'
     C                   move      ##pbiv        view#
X01  C                   else
     C                   eval      view# = 1
E01  C                   endif
     C*
     C* Dont allow add if client has Provider Bills Lite
     C*
     C                   call      'RTVCLTID'
     C                   parm                    ##clt#            3
     C*
B01  C                   if        ##clt# = '136'
     C                             or ##clt# = '141'
     C                             or ##clt# = '139'
     C*** Beg Add *** 03/20/13 *********************************************
     C                             or ##clt# = '046'
     C*** End Add *** 03/20/13 *********************************************
     C*** Beg Add *** 08/06/14 *********************************************
     C*** Beg Del *** 06/12/15 *********************************************
     C*****************            or ##clt# = '146'
     C*** End Del *** 06/12/15 *********************************************
     C*** End Add *** 08/06/14 *********************************************
     C*** Beg Add *** 09/28/15 *********************************************
     C                             or ##pblt = 'Y'
     C*** End Add *** 09/28/15 *********************************************
    C*
     C*   Disable function F6
    C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
E01  C                   endif
    C*
     C*** Beg Add ***** 11/22/16 ***************************
     C                   Eval      isBErr  = *Off
     C                   If        IsModEnabled('Line Validation')
     C                   Eval      isBErr  = *On
     C                   EndIf
     C*** End Add ***** 11/22/16 ***************************
    C*
B01  C                   if        ##clt# <> '114'
     C                   eval      *in20   = *on
    C*
     C*   Disable option 84
    C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '84'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
E01  C                   endif                                                  ##clt# <> '114'
    C*
    C*** BEG ADD *** 08/10/12 ****************************************
    C*
    C*   Disable option 40
    C*
B01  C                   if        ##clt# <> '144'
     C*** Beg Del *** 03/03/15 *********************************************
     C*****************  MOVE      ' '           @@CALL
     C*** End Del *** 03/03/15 *********************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '40'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
    C*
     C                   EXSR      UPDOFA
    C*
E01  C                   endif
    C*
    C*** END ADD *** 08/10/12 ****************************************
    C*
B01  C                   if        @@CALL = 'X'
     C*   Disable option 21
     C*** Beg Del *** 03/03/15 *********************************************
     C*****************  MOVE      ' '           @@CALL
     C*** End Del *** 03/03/15 *********************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '21'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
E01  C                   endif                                                  ##clt# <> '114'
    C*** Beg Add *** 11/13/12 ***************************************
      /Free
      /If Not Defined(WEB)
           XXOpfn = '1';  //These two fields remain the same for all options
           XXMIS = 'NNN'; //being conditioned here
       //Disable option 04
           XXNmbr = '04';
           Exsr UPDOFA;
       //Disable option 06
           XXNmbr = '06';
           Exsr UPDOFA;
       //Disable option 07
           XXNmbr = '07';
           Exsr UPDOFA;
       //Disable option 08
           XXNmbr = '08';
           Exsr UPDOFA;
       //Disable option 10
           XXNmbr = '10';
           Exsr UPDOFA;
       //Disable option 11
            XXNmbr = '11';
            Exsr UPDOFA;
       //Disable option 12
            XXNmbr = '12';
            Exsr UPDOFA;
       //Disable option 13
            XXNmbr = '13';
            Exsr UPDOFA;
       //Disable option 14
            XXNmbr = '14';
            Exsr UPDOFA;
       //Disable option 16
            XXNmbr = '16';
            Exsr UPDOFA;
       //Disable option 20
            XXNmbr = '20';
            Exsr UPDOFA;
       //Disable option 21
            XXNmbr = '21';
            Exsr UPDOFA;
       //Disable option 22
            XXNmbr = '22';
            Exsr UPDOFA;
       //Disable option 30
            XXNmbr = '30';
            Exsr UPDOFA;
       //Disable option 81
            XXNmbr = '81';
            Exsr UPDOFA;
       //Disable option 82
            XXNmbr = '82';
            Exsr UPDOFA;
      /EndIf
      /End-Free
    C*** End Add *** 11/13/12 ***************************************
     C***** Beg Add *** 02/27/20 **************************************
     C/free

       // default field search indicators to off
        srchClm = *off;
        srchProv = *off;
        srchIUsr = *off;

      /end-free
     C***** End Add *** 02/27/20 **************************************
    C*
     C                   endsr
    C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
    C*
     C     read          begsr
    C*
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
     C                   EVAL      DBRRN6=0
     C                   move      'N'           vldopt
    C*
B01  C                   select
    C*
S01  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key31         reade     dbrcdu                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key31         reade     dbrcdv                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key31         reade     dbrcdw                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key31         reade     dbrcdg                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key31         reade     dbrc07                                 95
E02  C                   endif
    C*
S02  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key22         reade     dbrcdm                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key22         reade     dbrcdn                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key22         reade     dbrcdo                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key22         reade     dbrcdp                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key22         reade     dbrc08                                 95
E02  C                   endif
    C*
S03  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key11         reade     dbrcdh                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key11         reade     dbrcdi                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key11         reade     dbrc06                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key11         reade     dbrcdi                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key11         reade     dbrc09                                 95
E02  C                   endif
    C*
S04  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key9          reade     dbrcdc                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key9          reade     dbrcdd                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key9          reade     dbrcde                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key9          reade     dbrcdg                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key9          reade     dbrc10                                 95
E02  C                   endif
    C*
S05  C                   when      ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key15         reade     dbrcdk                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key15         reade     dbrc05                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key15         reade     dbrcdl                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key15         reade     dbrc05                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key15         reade     dbrc11                                 95
E02  C                   endif
    C*
S06  C                   when      sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key35         reade     dbrcdx                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key35         reade     dbrcdy                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key35         reade     dbrcdz                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key35         reade     dbrcd4                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key35         reade     dbrc12                                 95
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key35         reade     dbrcdz                                 95
E02  C                   endif
    C*
S07  C                   when      sssub# <> *zeros and ssfnd <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key67         reade     dbrc25                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key67         reade     dbrc26                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key67         reade     dbrc27                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key67         reade     dbrc28                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key67         reade     dbrc29                                 95
E02  C                   endif
    C*
S08  C                   when      sssub# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key61         reade     dbrc20                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key61         reade     dbrc21                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key61         reade     dbrc22                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key61         reade     dbrc23                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key61         reade     dbrc24                                 95
E02  C                   endif
    C*
S09  C                   when      ssblst <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key40         reade     dbrct1                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key40         reade     dbrct2                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key40         reade     dbrct3                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key40         reade     dbrct4                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key40         reade     dbrc13                                 95
E02  C                   endif
    C*
S10  C                   when      ssbtyp <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key55         reade     dbrc15                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key55         reade     dbrc16                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key55         reade     dbrc17                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key55         reade     dbrc18                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key55         reade     dbrc19                                 95
E02  C                   endif
    C*
S11  C                   when      ssiusr <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key27         reade     dbrcdq                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key27         reade     dbrcdr                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key27         reade     dbrcds                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key27         reade     dbrcdt                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key27         reade     dbrc14                                 95
E02  C                   endif
    C*
S12  C                   other
    C*
B02  C                   if        *in21 = *on
     C     key           reade     dbrcdf                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key           reade     dbrcd2                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key           reade     dbrcd3                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key           reade     dbrcd4                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key           reade     dbrcd5                                 95
E02  C                   endif
B02  C                   if        *in26 = *on
     C     key           reade     dbrc30                                 95
E02  C                   endif
    C*
E01  C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
    C*
     C     readp         begsr
    C*
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
     C                   EVAL      DBRRN6=0
    C*
     C                   move      'N'           vldopt
    C*
B01  C                   select
    C*
S01  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key31         readpe    dbrcdu                                 95
E02  C                   endif
B02  C                   if        *in22 = *on
     C     key31         readpe    dbrcdv                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key31         readpe    dbrcdw                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key31         readpe    dbrcdg                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key31         readpe    dbrc07                                 95
E02  C                   endif
    C*
S02  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key22         readpe    dbrcdm                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key22         readpe    dbrcdn                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key22         readpe    dbrcdo                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key22         readpe    dbrcdp                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key22         readpe    dbrc08                                 95
E02  C                   endif
    C*
S03  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key11         readpe    dbrcdh                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key11         readpe    dbrcdi                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key11         readpe    dbrc06                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key11         readpe    dbrcdi                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key11         readpe    dbrc09                                 95
E02  C                   endif
    C*
S04  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key9          readpe    dbrcdc                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key9          readpe    dbrcdd                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key9          readpe    dbrcde                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key9          readpe    dbrcdg                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key9          readpe    dbrc10                                 95
E02  C                   endif
    C*
S05  C                   when      ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key15         readpe    dbrcdk                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key15         readpe    dbrc05                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key15         readpe    dbrcdl                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key15         readpe    dbrc05                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key15         readpe    dbrc11                                 95
E02  C                   endif
    C*
S06  C                   when      sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key35         readpe    dbrcdx                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key35         readpe    dbrcdy                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key35         readpe    dbrcdz                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key35         readpe    dbrcd4                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key35         readpe    dbrc12                                 95
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key35         readpe    dbrcdz                                 95
E02  C                   endif
    C*
S07  C                   when      sssub# <> *zeros and ssfnd <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key67         readpe    dbrc25                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key67         readpe    dbrc26                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key67         readpe    dbrc27                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key67         readpe    dbrc28                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key67         readpe    dbrc29                                 95
E02  C                   endif
    C*
S08  C                   when      sssub# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key61         readpe    dbrc20                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key61         readpe    dbrc21                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key61         readpe    dbrc22                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key61         readpe    dbrc23                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key61         readpe    dbrc24                                 95
E02  C                   endif
    C*
S09  C                   when      ssblst <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key40         readpe    dbrct1                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key40         readpe    dbrct2                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key40         readpe    dbrct3                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key40         readpe    dbrct4                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key40         readpe    dbrc13                                 95
E02  C                   endif
    C*
S10  C                   when      ssbtyp <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key55         readpe    dbrc15                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key55         readpe    dbrc16                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key55         readpe    dbrc17                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key55         readpe    dbrc18                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key55         readpe    dbrc19                                 95
E02  C                   endif
    C*
S11  C                   when      ssiusr <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key27         readpe    dbrcdq                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key27         readpe    dbrcdr                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key27         readpe    dbrcds                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key27         readpe    dbrcdt                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key27         readpe    dbrc14                                 95
E02  C                   endif
    C*
S12  C                   other
    C*
B02  C                   if        *in21 = *on
     C     key           readpe    dbrcdf                                 95
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key           readpe    dbrcd2                                 95
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key           readpe    dbrcd3                                 95
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key           readpe    dbrcd4                                 95
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key           readpe    dbrcd5                                 95
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key           readpe    dbrc30                                 95
E02  C                   endif
    C*
E01  C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
    C*
     C     setgt         begsr
    C*
B01  C                   select
    C*
S01  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key28         setgt     dbrcdu                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key29         setgt     dbrcdv                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key30         setgt     dbrcdw                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key31         setgt     dbrcdg                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key41         setgt     dbrc07                             96
E02  C                   endif
    C*
S02  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key18         setgt     dbrcdm                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key19         setgt     dbrcdn                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key20         setgt     dbrcdo                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key21         setgt     dbrcdp                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key42         setgt     dbrc08                             96
E02  C                   endif
    C*
S03  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key10         setgt     dbrcdh                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key11a        setgt     dbrcdi                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key12         setgt     dbrc06                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key13         setgt     dbrcdi                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key43         setgt     dbrc09                             96
E02  C                   endif
    C*
S04  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key5          setgt     dbrcdc                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key6          setgt     dbrcdd                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key7          setgt     dbrcde                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key8          setgt     dbrcdg                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key44         setgt     dbrc10                             96
E02  C                   endif
    C*
S05  C                   when      ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key14         setgt     dbrcdk                             96
E02  C                   endif
B02  C                   if        *in22 = *on
     C     key15a        setgt     dbrc05                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key16         setgt     dbrcdl                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key17         setgt     dbrc05                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key45         setgt     dbrc11                             96
E02  C                   endif
    C*
S06  C                   when      sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key32         setgt     dbrcdx                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key33         setgt     dbrcdy                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key34         setgt     dbrcdz                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key35         setgt     dbrcd4                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key46         setgt     dbrc12                             96
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key34         setgt     dbrcdz                             96
E02  C                   endif
    C*
S07  C                   when      sssub# <> *zeros and ssfnd <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key62         setgt     dbrc25                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key63         setgt     dbrc26                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key64         setgt     dbrc27                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key65         setgt     dbrc28                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key66         setgt     dbrc29                             96
E02  C                   endif
    C*
S08  C                   when      sssub# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key56         setgt     dbrc20                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key57         setgt     dbrc21                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key58         setgt     dbrc22                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key59         setgt     dbrc23                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key60         setgt     dbrc24                             96
E02  C                   endif
    C*
S09  C                   when      ssblst <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key36         setgt     dbrct1                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key37         setgt     dbrct2                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key38         setgt     dbrct3                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key39         setgt     dbrct4                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key47         setgt     dbrc13                             96
E02  C                   endif
    C*
S10  C                   when      ssbtyp <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key50         setgt     dbrc15                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key51         setgt     dbrc16                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key52         setgt     dbrc17                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key53         setgt     dbrc18                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key54         setgt     dbrc19                             96
E02  C                   endif
    C*
S11  C                   when      ssiusr <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key23         setgt     dbrcdq                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key24         setgt     dbrcdr                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key25         setgt     dbrcds                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key26         setgt     dbrcdt                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key48         setgt     dbrc14                             96
E02  C                   endif
    C*
S12  C                   other
    C*
B02  C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key2          setgt     dbrcd2                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key3          setgt     dbrcd3                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key4          setgt     dbrcd4                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key49         setgt     dbrcd5                             96
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key68         setgt     dbrc30                             96
E02  C                   endif
    C*
E01  C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
    C*
     C     setll         begsr
    C*
B01  C                   select
    C*
S01  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key28         setll     dbrcdu                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key29         setll     dbrcdv                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key30         setll     dbrcdw                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key31         setll     dbrcdg                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key41         setll     dbrc07                             96
E02  C                   endif
    C*
S02  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key18         setll     dbrcdm                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key19         setll     dbrcdn                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key20         setll     dbrcdo                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key21         setll     dbrcdp                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key42         setll     dbrc08                             96
E02  C                   endif
    C*
S03  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key10         setll     dbrcdh                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key11a        setll     dbrcdi                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key12         setll     dbrc06                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key13         setll     dbrcdi                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key43         setll     dbrc09                             96
E02  C                   endif
    C*
S04  C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key5          setll     dbrcdc                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key6          setll     dbrcdd                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key7          setll     dbrcde                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key8          setll     dbrcdg                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key44         setll     dbrc10                             96
E02  C                   endif
    C*
S05  C                   when      ssfein <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key14         setll     dbrcdk                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key15a        setll     dbrc05                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key16         setll     dbrcdl                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key17         setll     dbrc05                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key45         setll     dbrc11                             96
E02  C                   endif
    C*
S06  C                   when      sseob# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key32         setll     dbrcdx                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key33         setll     dbrcdy                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key34         setll     dbrcdz                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key35         setll     dbrcd4                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key46         setll     dbrc12                             96
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key34         setll     dbrcdz                             96
E02  C                   endif
    C*
S07  C                   when      sssub# <> *zeros and ssfnd <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key62         setll     dbrc25                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key63         setll     dbrc26                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key64         setll     dbrc27                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key65         setll     dbrc28                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key66         setll     dbrc29                             96
E02  C                   endif
    C*
S08  C                   when      sssub# <> *zeros
    C*
B02  C                   if        *in21 = *on
     C     key56         setll     dbrc20                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key57         setll     dbrc21                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key58         setll     dbrc22                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key59         setll     dbrc23                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key60         setll     dbrc24                             96
E02  C                   endif
    C*
S09  C                   when      ssblst <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key36         setll     dbrct1                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key37         setll     dbrct2                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key38         setll     dbrct3                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key39         setll     dbrct4                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key47         setll     dbrc13                             96
E02  C                   endif
    C*
S10  C                   when      ssbtyp <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key50         setll     dbrc15                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key51         setll     dbrc16                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key52         setll     dbrc17                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key53         setll     dbrc18                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key54         setll     dbrc19                             96
E02  C                   endif
    C*
S11  C                   when      ssiusr <> *blanks
    C*
B02  C                   if        *in21 = *on
     C     key23         setll     dbrcdq                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key24         setll     dbrcdr                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key25         setll     dbrcds                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key26         setll     dbrcdt                             96
E02  C                   endif
B02  C                   if        *in25 = *on
     C     key48         setll     dbrc14                             96
E02  C                   endif
    C*
S12  C                   other
    C*
B02  C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
E02  C                   endif
    C*
B02  C                   if        *in22 = *on
     C     key2          setll     dbrcd2                             96
E02  C                   endif
    C*
B02  C                   if        *in23 = *on
     C     key3          setll     dbrcd3                             96
E02  C                   endif
    C*
B02  C                   if        *in24 = *on
     C     key4          setll     dbrcd4                             96
E02  C                   endif
    C*
B02  C                   if        *in25 = *on
     C     key49         setll     dbrcd5                             96
E02  C                   endif
    C*
B02  C                   if        *in26 = *on
     C     key68         setll     dbrc30                             96
E02  C                   endif
    C*
E01  C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
    C*
     C     sf1upd        begsr
    C*
    C*
     C                   endsr
    C*
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
    C*
     C     slctrd        begsr
    C*
     C                   eval      @@eob# = sceob#
    C*
B01  C                   if        q1prms > 11
    C*
     C                   eval      @@rcn = scrcn
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SSCLER - Clear subset description fields
    C*================================================================
    C*
     C     sscler        begsr
    C*
    C* CLEAR ALL SUBSET DESCRIPTIONS IF CORRESPONDING SUBSET FIELD
    C* IS EMPTY
    C*
     C***** Beg Dlt *** 02/27/20 *******************************************
B01  C***                if        sssub# = *zeros
     C***                clear                   sssmnm
E01  C***                endif
     C***** End Dlt *** 02/27/20 *******************************************
    C*
     C***** Beg Dlt *** 02/27/20 *******************************************
B01  C***                if        ssfnd = *zeros
     C***                clear                   ssfnnm
E01  C***                endif
     C***** End Dlt *** 02/27/20 *******************************************
    C*
B01  C                   if        ssfyr = *zeros
     C                             and sscase = *zeros
     C                   clear                   ssname
E01  C                   endif
    C*
B01  C                   if        ssblst = *blanks
     C                   clear                   ssbsds
E01  C                   endif
    C*
B01  C                   if        ssfein = *zeros
     C*** Beg Chg *** 07/26/16 ********************************
     C***************************  and ssmod = *blanks
     C                             and ssmod = *zeros
     C*** End Chg *** 07/26/16 ********************************
     C                   clear                   sspdnm
E01  C                   endif
    C*
B01  C                   if        ssiusr = *blanks
     C                   clear                   ssusnm
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
    C*
     C     subset        begsr
    C*
     C                   move      eoco#         #1co#
     C                   move      eofnd         #1fnd
     C                   move      eosub#        #1sub#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
     C*** Check claim authority
    C*
     C                   move      #1wcf#        #1fnd
     C                   move      eofyr         #1fyr
     C                   move      eocase        #1case
     C                   move      *blanks       #1rtcd
     C*
     C                   call      'WA067R'      cuspl
    C*
     C* not authorised when rtcd = '2'
    C*
B01  C                   if        #1rtcd = '2'
     C                   eval      meetss = *off
E01  C                   endif
    C*
B01  C                   if        sssub# <> *zeros
     C                             and sssub# <> eosub#
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        ssidoc <> *blanks
    C*
     C     eaky2         chain     pea01                              99
    C*
B02  C                   if        not *in99
    C*
B03  C                   dow       not *in99
    C*
B04  C                   if        ssidoc <> *blanks and
     C                             ssidoc <> eaidoc
     C                   move      *off          meetss
X04  C                   else
     C                   move      *on           meetss
     C                   leave
E04  C                   endif
    C*
     C     eaky2         reade     pea01                                  99
    C*
E03  C                   enddo
    C*
X02  C                   else
     C                   move      *off          meetss
E02  C                   endif
    C*
E01  C                   endif
    C*
B01  C                   if        ssfnd <> *zeros
     C                             and ssfnd <> eofnd
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        ssfyr <> *zeros
     C                             and ssfyr <> eofyr
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        sscase <> *zeros
     C                             and sscase <> eocase
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        ssfein <> *zeros
     C                             and ssfein <> eofein
     C                   move      *off          meetss
E01  C                   endif
    C*
     C                   movel     ssmod         xxmod
     C*** Beg Chg *** 07/26/16 ********************************
B01  C*****************  if        ssmod <> *blanks
B01  C                   if        ssmod <> *zeros
     C*** End Chg *** 07/26/16 ********************************
     C                             and xxmod <> eomod
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        sseob# <> *zeros
     C                             and sseob# <> eoeob#
    C*
     C                   movel(p)  *off          meetss
    C*
E01  C                   endif
    C*
B01  C                   if        sscert <> *blanks
     C                             and sscert <> eocert
    C*
     C                   movel(p)  *off          meetss
    C*
E01  C                   endif
    C*
    C* SUBSET DATE OF SERVICE SHOULD FALL WITHIN EOMB SERVICE DATE
    C* RANGE (EOFRDT - EOTODT)
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
    C*
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlfrdt
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hltodt
    C*
    C* SUBSET CRITERIA WHEN ONLY FROM DATE IS ENTERED
    C*
B01  C                   if        hlfrdt > *zeros
     C                             and hltodt = *zeros
    C*
B02  C                   if        eofrdt < hlfrdt
     C                             and eotodt < hlfrdt
     C                   move      *off          meetss
E02  C                   endif
    C*
E01  C                   endif
    C*
    C* SUBSET CRITERIA WHEN RANGE IS SPECIFIED
    C*
B01  C                   if        hlfrdt > *zeros
     C                             and hltodt > *zeros
    C*
B02  C                   if        eotodt < hlfrdt
     C                             or eofrdt > hltodt
     C                   move      *off          meetss
E02  C                   endif
    C*
E01  C                   endif
    C*
B01  C                   if        ssblst <> *blanks
     C                             and ssblst <> eoblst
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        ssbtyp <> *blanks
     C                             and ssbtyp <> eobtyp
     C                   move      *off          meetss
E01  C                   endif
    C*
B01  C                   if        ssiusr <> *blanks
     C                             and ssiusr <> eoiusr
     C                   move      *off          meetss
E01  C                   endif
B01  C                   if        *in55 = *on
    C*
     C                   move      'A'           scauth
    C*
    C* Check Subscriber Restriction
    C*
B02  C                   if        ##mrsu = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
     C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      *zero         $$fnd
     C                   parm      *zero         #2emp#
     C                   parm      eosub#        $$sub#
    C*
B03  C                   if        ##rtcd = *on
     C                   move      *off          meetss
E03  C                   endif
E02  C                   endif
    C*
    C*  Check Group restriction
    C*
B02  C                   if        ##mrgr = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
    C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      eofnd         $$fnd
     C                   parm      *zero         #2emp#
     C                   parm      eosub#        $$sub#
    C*
B03  C                   if        ##rtcd = *on
     C                   move      *off          meetss
E03  C                   endif
E02  C                   endif
    C*
    C*  Check for Employer Restriction
    C*
B02  C                   if        ##mrgr = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
     C*
     C     cmky1         chain     pcm01                              99
B03  C                   if        *in99 = *off
     C*
     C                   call      'SA008R'                             99
     C                   parm                    ##co#
     C                   parm      q1user        ##usr
     C                   parm      'M'           ##appl
     C                   parm      *off          ##rtcd
     C     scauth        parm      *blank        ##auth
     C                   parm      cmfnd         $$fnd
     C                   parm      cmemp#        #2emp#
     C                   parm      cmsub#        $$sub#
    C*
B04  C                   if        ##rtcd = *on
     C                   move      *off          meetss
E04  C                   endif
    C*
E03  C                   endif
    C*
E02  C                   endif
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C*
** Column headings
Opt Claimant                EOMB#    Provider       From          To      Sts
Opt Provider       Claimant                EOMB#    From          To      Sts
Opt Soc Sec #     DOI        EOMB#   Provider       From          To      Sts
Opt    EOMB#   Claimant             Provider        From          To      Sts
Opt Control Number Claimant                EOMB#    From          To      Sts
Opt Image Doc ID   Claimant                EOMB#    From          To      Sts
Opt Claimant                EOMB#    Provider    Charge Amt   Paid Amt Typ AST
Opt Provider       Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Soc Sec #     DOI        EOMB#   Provider    Charge Amt   Paid Amt Typ AST
Opt    EOMB#   Claimant             Provider     Charge Amt   Paid Amt Typ AST
Opt Control Number Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Image Doc ID   Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Claimant                EOMB#    Provider     Control Number     Sts
Opt Provider       Claimant                EOMB#  Control Number     Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Control Number     Sts
Opt    EOMB#   Claimant             Provider      Control Number     Sts
Opt Control Number Claimant                EOMB#  Control Number     Sts
Opt Image Doc ID   Claimant                EOMB#  Control Number     Sts
Opt Claimant                EOMB#    Provider     Provider's Name         Sts
Opt Provider       Claimant                EOMB#  Provider's Name         Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Provider's Name         Sts
Opt    EOMB#   Claimant             Provider      Provider's Name         Sts
Opt Control Number Claimant                EOMB#  Provider's Name         Sts
Opt Image Doc ID   Claimant                EOMB#  Provider's Name         Sts
** Description--------------MISN
1011=Select                 NNY                                                          1
1022=Change                 YNN                                                          2
1033=Copy                   YNN     07/29/99                                             3
1044=Delete                 YNN                                                          4
1055=Display                YYY                                                          5
1066=Dtl Chrg List          YNN                                                          6
1077=Work w/ Detail         YYY                                                          7
1088=Work w/ Amounts        YYN                                                          8
1099=EOMB Delete            NNN                                                          9
11010=EOMB status           YYY                                                         10
11111=EOB/Notes             YYY                                                          1
11212=Print EOMB            YYN                                                          2
11313=Display EOMB          YYY                                                          3
11414=DARTS                 YNN                                                          4
11515=Pre-Cert Info         YYY                                                          5
11616=History               YYY                                                          6
11717=T/M Activity          NNN                                                          7
11818=EDI Log Info          YYN                                                          8
11919=Send Subsq 837        YYN                                                          9
12020=Pended items          YYY                                                         20
12121=Check info            YYN                                                          1
12222=View Prov Info        YYY                                                          2
13030=Letters               YYN                                                          3
14040=Image Doc ID#         YYY          08/10/12                                        4
18181=Claim info            YYN                                                          5
18282=Case info             YYY                                                          6
18484=Diary Adjuster        YYN          04/19/06                                        7
203F3=Exit                  YYN                                                          8
205F5=Refresh               YYY                                                          9
206F6=Add                   YNN                                                         30
209F9=Change Info           YYN                                                          1
211F11=Change view          YYN                                                          2
212F12=Cancel               YYY                                                          3
215F15=Fold/Truncate        YYN                                                          4
217F17=Subset               YYY                                                          5
3                                                                                        6
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )   OVRSCOPE(*JOB)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )   OVRSCOPE(*JOB)
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )   OVRSCOPE(*JOB)
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )   OVRSCOPE(*JOB)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  ) OVRSCOPE(*JOB)
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  ) OVRSCOPE(*JOB)
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )   OVRSCOPE(*JOB)
DLTOVR FILE(FTABL)  LVL(*JOB)
DLTOVR FILE(FMPRL)  LVL(*JOB)
DLTOVR FILE(FTPSL)  LVL(*JOB)
DLTOVR FILE(FMSVP)  LVL(*JOB)
DLTOVR FILE(FMSVL1) LVL(*JOB)
DLTOVR FILE(FMSVL2) LVL(*JOB)
DLTOVR FILE(FMPRP)  LVL(*JOB)
