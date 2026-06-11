     H*****************************************************************
     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H/TITLE F0359R - Work with Pended Provider Bills by Upload
     H*****************************************************************
     H*  Created: 08/08/05  C:79 Project: 135MET from F0358R          *
     H*                                                               *
    H*****************************************************************
    H** REVISIONS:
     H*
     H*  06/09/20 C:295  Project : SY420
     H*                  Mask SSN
     H*
     H*  05/25/07 C:191 PROJECT: WC1515
     H*                 Check claims authority
    H*
    H*  08/08/06 C:79  SUPPORT: 53486
    H*                 ADD 12=PRINT
    H*
    H*  12/12/05 C:79  SUPPORT: 50090
    H*                 ADD 9=COMPLETE
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
    H*
      /IF DEFINED (WEB)
     H******* DFTACTGRP(*NO)
      /ENDIF
     Ff0359f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffaeop     if   e             disk
     F                                     rename(peo01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
    F*** BEGIN CHG *** 10/29/02 *******************************************
VW#1 F**********faeol69   if   e           k disk
VW#1 Ffaeol69   if   e           k disk    usropn
    F*** END   CHG *** 10/29/02 *******************************************
     F                                     infds(dbfds)
     F                                     rename(peo01:dbrcdf)
    F*** BEGIN CHG *** 10/29/02 *******************************************
VW#2 F**********faeol70   if   e           k disk
VW#2 Ffaeol70   if   e           k disk    usropn
    F*** END   CHG *** 10/29/02 *******************************************
     F                                     infds(dbfds2)
     F                                     rename(peo01:dbrcd2)
    F*** BEGIN CHG *** 10/29/02 *******************************************
VW#3 F**********faeol71   if   e           k disk
VW#3 Ffaeol71   if   e           k disk    usropn
    F*** END   CHG *** 10/29/02 *******************************************
     F                                     infds(dbfds3)
     F                                     rename(peo01:dbrcd3)
    F*** BEGIN CHG *** 10/29/02 *******************************************
VW#4 F**********faeol72   if   e           k disk
VW#4 Ffaeol72   if   e           k disk    usropn
    F*** END   CHG *** 10/29/02 *******************************************
     F                                     infds(dbfds4)
     F                                     rename(peo01:dbrcd4)
     Ffaeoln    if   e           k disk
     F                                     infds(dbfdsc)
     F                                     rename(peo01:dbrcdc)
     Ffaeolo    if   e           k disk
     F                                     infds(dbfdsd)
     F                                     rename(peo01:dbrcdd)
     Ffaeolp    if   e           k disk
     F                                     infds(dbfdse)
     F                                     rename(peo01:dbrcde)
     Ffaeolq    if   e           k disk
     F                                     infds(dbfdsg)
     F                                     rename(peo01:dbrcdg)
    F*** BEGIN CHG *** 04/27/99 **************************************
    F**********FAEOLR  IF  E           K        DISK
     Ffaeol25   if   e           k disk
    F*** END   CHG *** 04/27/99 **************************************
     F                                     infds(dbfdsh)
     F                                     rename(peo01:dbrcdh)
    F*** BEGIN CHG *** 04/27/99 **************************************
    F**********FAEOLS  IF  E           K        DISK
     Ffaeol27   if   e           k disk
    F*** END   CHG *** 04/27/99 **************************************
     F                                     infds(dbfdsi)
     F                                     rename(peo01:dbrcdi)
    F*** BEGIN DEL *** 04/27/99 **************************************
    F**********FAEOLT  IF  E           K        DISK
    F**********                                    KINFDS DBFDSJ
    F**********  PEO01                             KRENAMEDBRCDJ
    F*** END   DEL *** 04/27/99 **************************************
    F*** BEGIN CHG *** 04/27/99 **************************************
    F**********FAEOLU  IF  E           K        DISK
     Ffaeol28   if   e           k disk
    F*** END   CHG *** 04/27/99 **************************************
     F                                     infds(dbfdsk)
     F                                     rename(peo01:dbrcdk)
    F*** BEGIN CHG *** 04/27/99 **************************************
    F**********FAEOLV  IF  E           K        DISK
     Ffaeol30   if   e           k disk
    F*** END   CHG *** 04/27/99 **************************************
     F                                     infds(dbfdsl)
     F                                     rename(peo01:dbrcdl)
     Ffaeolw    if   e           k disk
     F                                     infds(dbfdsm)
     F                                     rename(peo01:dbrcdm)
     Ffaeolx    if   e           k disk
     F                                     infds(dbfdsn)
     F                                     rename(peo01:dbrcdn)
     Ffaeoly    if   e           k disk
     F                                     infds(dbfdso)
     F                                     rename(peo01:dbrcdo)
     Ffaeolz    if   e           k disk
     F                                     infds(dbfdsp)
     F                                     rename(peo01:dbrcdp)
     Ffaeol10   if   e           k disk
     F                                     infds(dbfdsq)
     F                                     rename(peo01:dbrcdq)
     Ffaeol11   if   e           k disk
     F                                     infds(dbfdsr)
     F                                     rename(peo01:dbrcdr)
     Ffaeol12   if   e           k disk
     F                                     infds(dbfdss)
     F                                     rename(peo01:dbrcds)
     Ffaeol13   if   e           k disk
     F                                     infds(dbfdst)
     F                                     rename(peo01:dbrcdt)
     Ffaeol14   if   e           k disk
     F                                     infds(dbfdsu)
     F                                     rename(peo01:dbrcdu)
     Ffaeol15   if   e           k disk
     F                                     infds(dbfdsv)
     F                                     rename(peo01:dbrcdv)
     Ffaeol16   if   e           k disk
     F                                     infds(dbfdsw)
     F                                     rename(peo01:dbrcdw)
     Ffaeol17   if   e           k disk
     F                                     infds(dbfdsx)
     F                                     rename(peo01:dbrcdx)
     Ffaeol18   if   e           k disk
     F                                     infds(dbfdsy)
     F                                     rename(peo01:dbrcdy)
     Ffaeol19   if   e           k disk
     F                                     infds(dbfdsz)
     F                                     rename(peo01:dbrcdz)
    F*** BEGIN ADD *** 04/27/99 **************************************
     Ffaeol29   if   e           k disk
     F                                     infds(dbfdt5)
     F                                     rename(peo01:dbrc05)
     Ffaeol26   if   e           k disk
     F                                     infds(dbfdt6)
     F                                     rename(peo01:dbrc06)
    F*** END   ADD *** 04/27/99 **************************************
    F*** BEG ADD *** 03/08/00 ****************************************
     Ffaeol37   if   e           k disk
     F                                     infds(dbfd07)
     F                                     rename(peo01:dbrc07)
     Ffaeol38   if   e           k disk
     F                                     infds(dbfd08)
     F                                     rename(peo01:dbrc08)
     Ffaeol39   if   e           k disk
     F                                     infds(dbfd09)
     F                                     rename(peo01:dbrc09)
     Ffaeol40   if   e           k disk
     F                                     infds(dbfd10)
     F                                     rename(peo01:dbrc10)
     Ffaeol41   if   e           k disk
     F                                     infds(dbfd11)
     F                                     rename(peo01:dbrc11)
     Ffaeol42   if   e           k disk
     F                                     infds(dbfd12)
     F                                     rename(peo01:dbrc12)
     Ffaeol44   if   e           k disk
     F                                     infds(dbfd14)
     F                                     rename(peo01:dbrc14)
    F*** BEGIN CHG *** 10/29/02 *******************************************
VW#5 F**********faeol73   if   e           k disk
VW#5 Ffaeol73   if   e           k disk    usropn
    F*** END   CHG *** 10/29/02 *******************************************
     F                                     infds(dbfds5)
     F                                     rename(peo01:dbrcd5)
    F*** END ADD *** 03/08/00 ****************************************
    F*** BEGIN ADD *** 06/01/01 **************************************
VW#1 Ffaeol46   if   e           k disk
     F                                     infds(dbfd15)
     F                                     rename(peo01:dbrc15)
VW#2 Ffaeol47   if   e           k disk
     F                                     infds(dbfd16)
     F                                     rename(peo01:dbrc16)
VW#3 Ffaeol48   if   e           k disk
     F                                     infds(dbfd17)
     F                                     rename(peo01:dbrc17)
VW#4 Ffaeol49   if   e           k disk
     F                                     infds(dbfd18)
     F                                     rename(peo01:dbrc18)
VW#5 Ffaeol50   if   e           k disk
     F                                     infds(dbfd19)
     F                                     rename(peo01:dbrc19)
    F*** END   ADD *** 06/01/01 **************************************
    F*** BEGIN ADD *** 05/30/02 **************************************
VW#1 Ffaeol56   if   e           k disk
     F                                     infds(dbfd20)
     F                                     rename(peo01:dbrc20)
VW#2 Ffaeol57   if   e           k disk
     F                                     infds(dbfd21)
     F                                     rename(peo01:dbrc21)
VW#3 Ffaeol58   if   e           k disk
     F                                     infds(dbfd22)
     F                                     rename(peo01:dbrc22)
VW#4 Ffaeol59   if   e           k disk
     F                                     infds(dbfd23)
     F                                     rename(peo01:dbrc23)
VW#5 Ffaeol60   if   e           k disk
     F                                     infds(dbfd24)
     F                                     rename(peo01:dbrc24)
VW#1 Ffaeol61   if   e           k disk
     F                                     infds(dbfd25)
     F                                     rename(peo01:dbrc25)
VW#2 Ffaeol62   if   e           k disk
     F                                     infds(dbfd26)
     F                                     rename(peo01:dbrc26)
VW#3 Ffaeol63   if   e           k disk
     F                                     infds(dbfd27)
     F                                     rename(peo01:dbrc27)
VW#4 Ffaeol64   if   e           k disk
     F                                     infds(dbfd28)
     F                                     rename(peo01:dbrc28)
VW#5 Ffaeol65   if   e           k disk
     F                                     infds(dbfd29)
     F                                     rename(peo01:dbrc29)
    F*** END   ADD *** 05/30/02 **************************************
    F*
    F*  Provider Demographics File
    F***FMPDL   IF  E           K        DISK
    F*
    F*  Subscriber Master File
    F***FMSMP   IF  E                    DISK
    F*
    F*  Group Master FIle
    F***FMFNP   IF  E                    DISK
    F*
    F*  Generic Table FIle
    F***FTGTP   IF  E                    DISK
    F*
    F*  User Parameter File
    F*** BEGIN DEL *** 06/01/01 **************************************
    F**********SMUPP   IF  E                    DISK
    F*** END   DEL *** 06/01/01 **************************************
    F*
    F*  Claim Master File
     Ffmcml1    if   e           k disk
    F*** BEGIN ADD *** 04/08/99 **************************************
     F                                     infds(dbfcm1)
    F*** END ADD *** 04/08/99 ****************************************
    F*
    F*  EOMB Status Change Transaction File
    F*** BEGIN CHG *** 05/30/02 **************************************
     F**********faesl     if   e           k disk
     Ffaesl     if   e           k disk
     F                                     infds(dbfda)
    F*** END   CHG *** 05/30/02 **************************************
    F*** BEGIN ADD *** 06/14/02 **************************************
    Ffaeal1    if   e           k disk
    F*** END   ADD *** 06/14/02 **************************************
    F*
    F*  Pre-Certification Master File
    F***UMPCL11 IF  E           K        DISK
    F***                                           KINFDS DBFDB
    F*  Subscriber Master File
    F***FMSML   IF  E           K        DISK
    F***         PSM01                             KRENAMEPSM02
    F*  WCMC Fund/Group Master File
    F***FMFNL   IF  E           K        DISK
    F***         PFN01                             KRENAMEPFN02
    F*  Generic Table  File
    F***FTGTL   IF  E           K        DISK
    F***         PGT01                             KRENAMEPGT02
    F*  User Parameter File
    F*** BEGIN DEL *** 06/01/01 **************************************
    F**********SMUPL   IF  E           K        DISK
    F**********  PUP01                             KRENAMEPUP02
    F*** END   DEL *** 06/01/01 **************************************
    F***FMBEL   IF  E           K        DISK
    F*** BEG ADD *** 05/07/99 ****************************************
    F**********FAEAL1  IF  E           K        DISK
    F*** END ADD *** 05/07/99 ****************************************
    F*** BEG ADD *** 09/10/99 ****************************************
    F***FELGL4  IF  E           K        DISK
    F*** END ADD *** 09/10/99 ****************************************
    F*** BEG ADD *** 05/18/00 ****************************************
    F*** BEGIN DEL *** 06/01/01 **************************************
    F*WDPALA  IF  E           K        DISK                          UC
    F*** END   DEL *** 06/01/01 **************************************
    F*** END ADD *** 05/18/00 ****************************************
    F*** BEGIN ADD *** 05/30/02 **************************************
    Fsmupl     if   e           k disk
    F*** END   ADD *** 05/30/02 **************************************
     F*** BEGIN ADD *** 10/29/02 *******************************************
     Ffmsml     if   e           k disk
     Ffmfnl1    if   e           k disk
     Ffmfnl     if   e           k disk
     F                                     rename(pfn01:pfn02)
     F*** END   ADD *** 10/29/02 *******************************************
    D*
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
    D*** BEG CHG *** 03/08/00 ****************************************
    D************        HD      1   8 78               Column headings
     D hd              s             78    dim(20) ctdata perrcd(1)
    D*** END CHG *** 03/08/00 ****************************************
    D*** BEGIN CHG *** 04/08/99 **************************************
    D********            OF      1  28  1   OFD    35   Options, Function
    D*** BEGIN CHG *** 07/29/99 **************************************
    D********            OF      1  32  1   OFD    35   Options, Function
     D*** BEG CHG *** 12/12/05 ****************************************
     D*** of              s              1    dim(33) ctdata perrcd(1)
     D*** ofd             s             35    dim(33) alt(of)
     D of              s              1    dim(34) ctdata perrcd(1)
     D ofd             s             35    dim(34) alt(of)
     D*** END CHG *** 12/12/05 ****************************************
    D*** END CHG *** 07/29/99 ****************************************
    D*** END CHG *** 04/08/99 ****************************************
    D*** BEGIN ADD *** 10/29/02 **************************************
     D grp             s              3    dim(999)
    D*** END   ADD *** 10/29/02 **************************************
    D*
    D*** BEG CHG *** 05/12/99 ****************************************
     D qca             s             56    dim(14) ctdata perrcd(1)
    D******************  QCA     1  12 56               CMD EXEC/QCMDEXC
    D*** END CHG *** 05/12/99 ****************************************
     D*** BEG ADD *** 08/08/05 ****************************************
     D ##kcif          s              1
     D ##curs          s              1
     D ##pcib          s              1
     D ##clt#          s              3
     D*** END ADD *** 08/08/05 ****************************************
    D*
     D data            s             74    dim(5)
     D dta1            s              1    dim(74)
     D dta2            s              1    dim(74)
     D dta3            s              1    dim(74)
     D dta4            s              1    dim(74)
    D*** BEG ADD *** 03/08/00 ****************************************
     D dta5            s              1    dim(74)
    D*** END ADD *** 03/08/00 ****************************************
     D inf1            s              1    dim(29)
     D inf2            s              1    dim(29)
    D*** BEG ADD *** 09/27/00 ****************************************
     D inf3            s              1    dim(29)
     D inf4            s              1    dim(29)
    D*** END ADD *** 09/27/00 ****************************************
    D*
    D*
     D dbfda           ds
     D dbrrna                397    400b 0
    D***DBFDB       DS
    D***                                 B 397 4000DBRRNB
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
    D*** BEGIN ADD *** 04/08/99 **************************************
     D dbfcm1          ds
     D dbrcm1                397    400b 0
    D*** END ADD *** 04/08/99 ****************************************
    D*** BEGIN ADD *** 04/27/99 **************************************
     D dbfdt5          ds
     D dbrr05                397    400b 0
     D dbfdt6          ds
     D dbrr06                397    400b 0
    D*** END   ADD *** 04/27/99 **************************************
    D*** BEG ADD *** 03/08/00 ****************************************
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
    D***DBFDS5      DS
    D***                                 B 397 4000DBRRN5
    D*** END ADD *** 03/08/00 ****************************************
    D*** BEGIN ADD *** 06/01/01 **************************************
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
    D*** END   ADD *** 06/01/01 **************************************
    D*** BEGIN ADD *** 05/30/02 **************************************
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
    D*** END   ADD *** 05/30/02 **************************************
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D qqcmd                         56
     D  qqst                          2    overlay(qqcmd:38)
    D*** BEG ADD *** 10/27/99 ****************************************
     D                 ds
     D qqcmd2                        56
     D  qqst2                         2    overlay(qqcmd2:40)
    D*** END ADD *** 10/27/99 ****************************************
    D*** BEGIN ADD *** 01/11/99 **************************************
     D                 ds
     D x1dt                          10
     D  x1dtm                         2    overlay(x1dt)
     D  x1dtd                         2    overlay(x1dt:4)
     D  x1dty                         4    overlay(x1dt:7)
     D                 ds
     D  x2dt                   1      8
     D  x2dtm                  1      2
     D  x2dtd                  3      4
     D  x2dty                  5      8
    D*** END ADD *** 01/11/99 ****************************************
    D*** BEGIN ADD *** 10/29/02 **************************************
     D qryslt          ds          5000
    D*** END   ADD *** 10/29/02 **************************************
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('Pended Concentra Bills')
     D slmsgi          c                   const('SYS0001')
     D slmsgf          c                   const('FSMESG')
     D acdpgm          c                   const('F0350R01')
     D comapi          c                   const('FA007R')
     D uspapi          c                   const('F0006R')
    D*** BEGIN ADD *** 10/29/02 **************************************
     D qrypgm          c                   const('F0350C1')
    D*** END   ADD *** 10/29/02 **************************************
     D infomx          c                   const(4)
     D viewmx          c                   const(5)
     D sf1pag          c                   const(8)
    D*================================================================
    D*  Program specific constants
    D*================================================================
     D heada           c                   const('Dates of Service')
     D clropt          c                   const('                    -
     D                                            NNN')
     D pciopt          c                   const('1515=Pre-Cert Info  -
     D                                            YYN')
    D*** BEGIN CHG *** 04/08/99 **************************************
     D clmopt          c                   const('8181=Claim info     -
     D                                            YYN')
    D*** END CHG *** 04/08/99 ****************************************
     D ediopt          c                   const('1818=EDI Log Info   -
     D                                            YYN')
     D ssqopt          c                   const('1919=Send Subsq 837 -
     D                                            YNN')
     D vckopt          c                   const('2121=Check info     -
     D                                            YYN')
     D delete          c                   const('Deleted')
    D*** BEGIN ADD *** 10/29/02 **************************************
     D co#             c                   const('(EOCO# *EQ')
     D co#2            c                   const(') *AND (')
     D sub#            c                   const('(EOSUB# *EQ')
     D fnd             c                   const('*AND (EOFND *EQ -
     D                                     %VALUES(')
     D ovrdb1          c                   const('OVRDBF FILE(')
     D ovrdb2          c                   const(') SHARE(*YES)')
    D** **         'SEQONLY(*NO)'
    D*** END   ADD *** 10/29/02 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
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
     D kkeob#          s                   like(eoeob#)
     D kkext           s              3  0
     D kkfein          s                   like(eofein)
     D kkfnd           s                   like(cmfnd)
     D kkfrdt          s                   like(eofrdt)
     D kkfsn           s                   like(eofsn)
     D kkfyr           s                   like(cmfyr)
     D*** BEGIN ADD *** 05/23/02 *******************************************
     D kkidt           s                   like(esidt)
     D kkitim          s                   like(esitim)
     D*** END   ADD *** 05/23/02 *******************************************
     D kkkey           s             10
     D kklsn           s                   like(eolsn)
     D kkmod           s                   like(eomod)
     D kkpnam          s                   like(eopnam)
     D kkrcn           s                   like(eorcn)
     D kkss#           s                   like(eoss#)
     D kksub#          s                   like(cmsub#)
     D kktbid          s              4
    D*** BEGIN ADD *** 05/30/02 **************************************
     D kkusr           s                   like(upusr)
    D*** END   ADD *** 05/30/02 **************************************
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
    D*** BEGIN ADD *** 10/29/02 **************************************
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
    D*** END   ADD *** 10/29/02 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C/COPY SCOPYWW01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
    C*** BEGIN ADD *** 12/27/98 **************************************
     C                   parm                    @@eob#
     C                   parm                    @@cert
    C*** END   ADD *** 12/27/98 **************************************
    C*** BEGIN ADD *** 01/11/99 **************************************
     C                   parm                    @@sfdt
     C                   parm                    @@stdt
     C                   parm                    @@sfei
     C                   parm                    @@smod
    C*** END ADD *** 01/11/99 ****************************************
    C*** BEGIN ADD *** 04/03/00 **************************************
     C                   parm                    @@rcn
    C*** END   ADD *** 04/03/00 **************************************
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
    C*** BEG ADD *** 09/10/99 ****************************************
     C                   parm                    ##nwst
    C*** END ADD *** 09/10/99 ****************************************
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*** BEGIN ADD *** 10/29/02 **************************************
    C*
     C     rstpl         plist
     C                   parm                    ##co#
     C                   parm      q1user        ##usr            10
     C                   parm      'M'           ##appl            1
     C                   parm      *off          ##rcds            1
    C*
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
    C*** END   ADD *** 10/29/02 **************************************
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** parameter list for claims secuirty API
     C     cuspl         plist
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      q1user        ##usr
     C                   parm                    #1rtcd
     C*** END ADD ****** 05/25/07 ********************************************
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
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
     C                   kfld                    kkeob#
    C*
     C     key11         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
    C*** BEGIN ADD *** 04/27/99 **************************************
    C*
     C     key11a        klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*** END   ADD *** 04/27/99 **************************************
    C*
     C     key12         klist
     C                   kfld                    ##co#
     C                   kfld                    sssub#
     C                   kfld                    ssfnd
     C                   kfld                    ssfyr
     C                   kfld                    sscase
     C                   kfld                    ssfein
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
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
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
     C                   kfld                    kkeob#
    C*
     C     key14         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
     C                   kfld                    kkeob#
    C*
     C     key15         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
    C*** BEGIN ADD *** 04/27/99 **************************************
    C*
     C     key15a        klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
     C                   kfld                    kkeob#
    C*** END   ADD *** 04/27/99 **************************************
    C*
     C     key16         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
     C                   kfld                    kkss#
     C                   kfld                    kkacdt
     C                   kfld                    kkeob#
    C*
     C     key17         klist
     C                   kfld                    ##co#
     C                   kfld                    ssfein
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           KFLD           SXMOD
    C*** END   DEL *** 04/27/99 **************************************
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
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** BEG ADD *** 03/08/00 ****************************************
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
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C     key50         klist
     C                   kfld                    ##co#
     C                   kfld                    ssbtyp
     C                   kfld                    kklsn
     C                   kfld                    kkfsn
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   kfld                    kkfrdt
    C*** END   ADD *** 09/07/01 **************************************
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
    C*** END   ADD *** 06/01/01 **************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
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
    C*** END   ADD *** 05/30/02 **************************************
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
     C*** BEGIN *** 10/29/02 ***********************************************
    C*
     C     cmky1         klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C*** END   *** 10/29/02 ***********************************************
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
     C*** BEGIN ADD *** 05/23/02 *******************************************
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
     C*** END   ADD *** 05/23/02 *******************************************
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
    C*** BEGIN DEL *** 06/01/01 **************************************
    C*** BEG ADD *** 05/18/00 ****************************************
    C*
    C* key list for WCMS payment detail file
    C*
    C********** PAKY      KLIST
    C**********           KFLD           ##CO#
    C**********           KFLD           KKFND
    C**********           KFLD           KKFYR
    C**********           KFLD           KKCASE
    C**********           KFLD           KKEOB#
    C*** END ADD *** 05/18/00 ****************************************
    C*** END   DEL *** 06/01/01 **************************************
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
    C*** BEGIN ADD *** 10/29/02 **************************************
     C     fnkey         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C*
     C     fnky3         klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
    C*** END   ADD *** 10/29/02 **************************************
    C*
    C* key list for generic table file
    C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*** BEGIN DEL *** 06/01/01 **************************************
    C*
    C* key list for user parameter file
    C*
    C********** UPKY      KLIST
    C**********           KFLD           KKUSR
    C*** END   DEL *** 06/01/01 **************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
    C*
    C* key list for user parameter file
    C*
    C     upky          klist
    C                   kfld                    kkusr
    C*** END   ADD *** 05/30/02 **************************************
    C*
    C* key list for Bill Entry Control File
    C*
     C     beky          klist
     C                   kfld                    scast
     C                   kfld                    scsys
     C                   kfld                    scbtyp
    C*
    C*** BEG ADD *** 05/07/99 ****************************************
    C**********
    C********** EAKY1     KLIST
    C**********           KFLD           ##CO#
    C**********           KFLD           KKSUB#
    C**********           KFLD           KKFND
    C**********           KFLD           KKFYR
    C**********           KFLD           KKCASE
    C**********           KFLD           KKEOB#
    C**********           KFLD           KKRCN#
    C**********
    C********** EAKY2     KLIST
    C**********           KFLD           ##CO#
    C**********           KFLD           KKSUB#
    C**********           KFLD           KKFND
    C**********           KFLD           KKFYR
    C**********           KFLD           KKCASE
    C**********           KFLD           KKEOB#
    C*** END ADD *** 05/07/99 ****************************************
    C*** BEGIN ADD *** 06/14/02 **************************************
    C*
    C     eaky          klist
    C                   kfld                    ##co#
    C                   kfld                    scsub#
    C                   kfld                    scfnd
    C                   kfld                    scfyr
    C                   kfld                    sccase
    C                   kfld                    sceob#
    C                   kfld                    scrcn
    C*** END   ADD *** 06/14/02 **************************************
    C*** BEG ADD *** 09/10/99 ****************************************
    C*** BEG DEL *** 03/08/00 ****************************************
    C*
    C*  KEY LIST FOR EDI LOG RECORD
    C********** LGKY1     KLIST
    C**********           KFLD           ##CO#
    C**********           KFLD           KKSUB#
    C**********           KFLD           KKFND
    C**********           KFLD           KKFYR
    C**********           KFLD           KKCASE
    C**********           KFLD           KKEOB#
    C**********           KFLD           KKRCN
    C**********           KFLD           KKLSEQ
    C*
    C********** LGKY2     KLIST
    C**********           KFLD           ##CO#
    C**********           KFLD           KKSUB#
    C**********           KFLD           KKFND
    C**********           KFLD           KKFYR
    C**********           KFLD           KKCASE
    C**********           KFLD           KKEOB#
    C**********           KFLD           KKRCN
    C*** BEG DEL *** 03/08/00 ****************************************
    C*** END ADD *** 09/10/99 ****************************************
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*** BEG DEL *** 03/08/00 ****************************************
    C********** *LIKE     DEFN GTTBID    ##TBID
    C*** END DEL *** 03/08/00 ****************************************
    C*** BEGIN CHG *** 06/01/01 **************************************
    C********** *LIKE     DEFN UPUSER    HLIUSR
    C*** END   CHG *** 06/01/01 **************************************
    C*** BEG DEL *** 03/08/00 ****************************************
    C********** *LIKE     DEFN PCCERT    KKCERT
    C********** *LIKE     DEFN PCEXT     KKEXT
    C********** *LIKE     DEFN GTTBID    KKTBID
    C********** *LIKE     DEFN GTKEY     KKKEY
    C*** END DEL *** 03/08/00 ****************************************
    C*** BEGIN DEL *** 06/01/01 **************************************
    C********** *LIKE     DEFN UPUSR     KKUSR
    C*** END   DEL *** 06/01/01 **************************************
    C*** BEG DEL *** 03/08/00 ****************************************
    C********** *LIKE     DEFN FNEXDT    HLSYDT
    C*** END DEL *** 03/08/00 ****************************************
    C*** BEGIN DEL *** 04/27/99 **************************************
    C********** *LIKE     DEFN EOMOD     SXMOD
    C*** END   DEL *** 04/27/99 **************************************
    C*** BEGIN CHG *** 06/01/01 **************************************
    C********** *LIKE     DEFN UPOUTQ    ##QUE
    C*** END   CHG *** 06/01/01 **************************************
    C*** BEG ADD *** 05/07/99 ****************************************
    C********** *LIKE     DEFN EARCN     KKRCN#
    C*** END ADD *** 05/07/99 ****************************************
    C*** BEG ADD *** 09/10/99 ****************************************
    C*** BEG DEL *** 03/08/00 ****************************************
    C********** *LIKE     DEFN LGLSEQ    KKLSEQ
    C*** END DEL *** 03/08/00 ****************************************
    C*** END ADD *** 09/10/99 ****************************************
    C*** BEG ADD *** 03/08/00 ****************************************
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 09/07/01 **************************************
    C*** END   ADD *** 09/07/01 **************************************
    C*
    C*
    C*================================================================
    C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
    C*================================================================
     C     assrrn        begsr
    C*
     C                   select
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnu
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnv
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrnw
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrng
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr07
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnm
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnn
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrno
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrnp
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr08
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21*****           Z-ADDDBRRNH    SCDBRR
    C***22*****           Z-ADDDBRRND    SCDBRR
    C***23*****           Z-ADDDBRRNI    SCDBRR
    C***24*****           Z-ADDDBRRNJ    SCDBRR
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnh
     C                   endif
    C*** BEG CHG *** 11/19/99 ****************************************
    C***22*****           Z-ADDDBRR06    SCDBRR
    C***23*****           Z-ADDDBRRNI    SCDBRR
    C***24*****           Z-ADDDBRR06    SCDBRR
    C*
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrni
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr06
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrni
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr09
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEG CHG *** 11/19/99 ****************************************
    C*** END   CHG *** 04/27/99 **************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnc
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnd
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrne
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrng
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr10
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21*****           Z-ADDDBRRNK    SCDBRR
    C***22*****           Z-ADDDBRRN2    SCDBRR
    C***23*****           Z-ADDDBRRNL    SCDBRR
    C***24*****           Z-ADDDBRRN2    SCDBRR
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnk
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr05
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrnl
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr05
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr11
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** END   CHG *** 04/27/99 **************************************
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnx
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrny
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrnz
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrn4
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr12
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
     C                   when      sssub# <> *zeros and ssfnd <> *zeros
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr25
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr26
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr27
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr28
     C                   endif
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr29
     C                   endif
     C*
     C                   when      sssub# <> *zeros
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr20
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr21
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr22
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr23
     C                   endif
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr24
     C                   endif
    C*
    C*** END   ADD *** 05/30/02 **************************************
    C*** BEGIN ADD *** 06/01/01 **************************************
     C                   when      ssbtyp <> *blanks
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrr15
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrr16
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrr17
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrr18
     C                   endif
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr19
     C                   endif
    C*
    C*** END   ADD *** 06/01/01 **************************************
     C                   when      ssiusr <> *blanks
     C                   if        *in21 = *on
     C                   eval      scdbrr = dbrrnq
     C                   endif
     C                   if        *in22 = *on
     C                   eval      scdbrr = dbrrnr
     C                   endif
     C                   if        *in23 = *on
     C                   eval      scdbrr = dbrrns
     C                   endif
     C                   if        *in24 = *on
     C                   eval      scdbrr = dbrrnt
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C                   eval      scdbrr = dbrr14
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
    C*
    C*** BEGIN ADD *** 12/27/98 **************************************
     C                   if        sssub# <> *zeros
     C                             or ssfnd <> *zeros
     C                             or ssfyr <> *zeros
     C                             or sscase <> *zeros
     C                             or sseob# <> *zeros
     C                             or sscert <> *blanks
    C*** BEGIN ADD *** 01/11/99 **************************************
     C                             or ssfrdt <> *zeros
     C                             or sstodt <> *zeros
     C                             or ssfein <> *zeros
     C                             or ssmod <> *blanks
    C*** END ADD *** 01/11/99 ****************************************
    C*** BEGIN ADD *** 06/01/01 **************************************
     C                             or ssbtyp <> *blanks
    C*** END   ADD *** 06/01/01 **************************************
    C*
     C                   eval      *in11 = *on
    C*
     C                   endif
    C*** END   ADD *** 12/27/98 **************************************
    C*
     C                   endsr
    C*================================================================
    C* DATSBR - Date check subrountine
    C*================================================================
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
     C     dlovrd        begsr
    C*** BEG CHG *** 05/12/99 ****************************************
     C                   eval      length = 56
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(8)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(9)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(10)
     C                   parm                    length
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(11)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(12)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(13)
     C                   parm                    length
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(14)
     C                   parm                    length
    C**********           CALL 'QCMDEXC'                60
    C**********           PARM           QCA,11
    C**********           PARM           LENGTH
    C**********           CALL 'QCMDEXC'                60
    C**********           PARM           QCA,12
    C**********           PARM           LENGTH
    C**********           CALL 'QCMDEXC'                60
    C**********           PARM           QCA,13
    C**********           PARM           LENGTH
    C**********           CALL 'QCMDEXC'                60
    C**********           PARM           QCA,14
    C**********           PARM           LENGTH
    C*** END CHG *** 05/12/99 ****************************************
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
    C*
     C                   move      'Y'           vldopt
    C*** BEGIN ADD *** 10/29/02 **************************************
     C                   select
    C*
    C*  Check for user authorization to OPT 2 = change.
    C*
     C*** BEG CHG *** 12/12/05 *********************************************
     C****************** when      scsel = '2'
     C                   when      (scsel = '2' or scsel = '9')
     C*** END CHG *** 12/12/05 *********************************************
     C                             and scauth = 'I'
     C                             and ##rcds = *on
     C                             and *in55 = *on
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
    C*
     C                   when      scauth = 'I'
     C                             and *in55 = *on
    C*
     C                   if        scsel = '3'
     C                             or scsel = '4'
     C                             or scsel = '6'
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   other
    C*** END   ADD *** 10/29/02 **************************************
    C*
    C* disallow certain options of bill status is delete
    C*
     C                   if        scblst = 'DEL'
     C                   if        scsel = '2'
     C                             or scsel = '4'
     C                             or scsel = '9'
    C********** SCSEL     OREQ '11'
     C                             or scsel = '12'
     C                             or scsel = '13'
     C                             or scsel = '17'
     C                             or scsel = '19'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
    C*
     C                   if        scblst = 'DEL'
     C                             and @@call <> '1'
     C                             and scsel = '7'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck1end
     C                   endif
    C*
    C* DO NOT CALL OPT15=PRE-CERT INFO IF NO PRECERT EXIST FOR BILL
    C*
     C                   if        scsel = '15'
     C     scdbrr        chain     pfrcdf                             98
     C                   if        *in98 = *off
     C                             and eocert = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C                   if        scsel = '2' or scsel = '4'
     C*
     C     eaky          chain     pea01                              99
     C                   if        *in99=*off and eansnt = 'S'
     C                   seton                                        7590
     C                   endif
     C*
     C                   endif
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*
    C* DELETE PROCESSING
    C*
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   eval      ##rrn = 0
     C                   if        scsel = '4'
    C*** BEG ADD *** 05/18/00 ****************************************
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
     C                   if        ##wcin = 'Y'
     C                             and ##rtcd = *blank
     C                             and ##outs <> 'Y'
    C*** BEGIN ADD *** 09/25/00 **************************************
     C                             and ##npwc <> 'Y'
    C*** END   ADD *** 09/25/00 **************************************
    C*** BEGIN ADD *** 11/10/00 **************************************
     C                             and ##wcif = 'Y'
    C*** END   ADD *** 11/10/00 **************************************
    C*
    C* When there is a check in history only allow option 4=Delete
    C* if the user has the proper security level.
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           Z-ADDSCFND     KKFND
    C**********           Z-ADDSCFYR     KKFYR
    C**********           Z-ADDSCCASE    KKCASE
    C**********           Z-ADDSCEOB#    KKEOB#
    C*
    C********** PAKY      CHAINPPA01                99
    C********** *IN99     IFEQ *OFF
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
     C                   if        ##rtcd <> 'F'
    C*** END   CHG *** 06/01/01 **************************************
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
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           MOVELQ1USER    KKUSR     P
    C*
    C********** UPKY      CHAINPUP02                99
    C*
    C********** *IN99     IFEQ *OFF
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
    C*** END   CHG *** 06/01/01 **************************************
     C                   call      'SA006R'
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           PARM           UPLVL
    C**********           PARM           UPTYPE
     C                   parm                    ##lvl
     C                   parm                    ##type
    C*** END   CHG *** 06/01/01 **************************************
    C*** BEGIN CHG *** 07/13/00 **************************************
    C**********           PARM           ##DNSL
    C**********           PARM           ##DNST
     C                   parm                    ##ckdl
     C                   parm                    ##ckdt
    C*** END   CHG *** 07/13/00 **************************************
     C                   parm      ' '           ##rtcd
    C*
     C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in88 = *on
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
    C*** END ADD *** 05/18/00 ****************************************
    C*
    C* IF AR IS INSTALLED, CALL WORK WITH EOMB INVOICES PROGRAM
    C*
     C                   if        ##ar = 'Y'
     C                   eval      ##rrn = scdbrr
     C                   move      ' '           ##call
     C                   eval      ##sub# = scsub#
     C                   eval      ##fnd = scfnd
     C                   eval      ##eob# = sceob#
     C                   eval      ##case = sccase
     C                   eval      ##fyr = scfyr
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
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                   move      '1'           rstpgm
     C                   clear                   scsel
    C*** BEGIN ADD *** 11/10/00 **************************************
     C                   move      *on           stop
    C*** END   ADD *** 11/10/00 **************************************
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*
     C                   movel     '99'          scsel
     C                   endif
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IF THE CORRESPONDING
    C* GROUP IS EXPIRED  (F9, F19)
    C*
     C                   if        scsel = '9'
     C                             or scsel = '19'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** FNKY      CHAINPFN02                99
    C********** *IN99     IFEQ *OFF
    C**********           TIME           DSDTTM
    C**********           Z-ADDDSDATE    SRDTI
    C**********           MOVE *BLANKS   SRDTO
    C**********           MOVE *BLANKS   SRERR
    C**********           CALL 'SRCVTDAT'DTCYMD
    C**********           MOVELSRDTO     HLSYDT
    C********** FNEXDT    IFNE *ZEROS
    C********** FNEXDT    ANDLTHLSYDT
    C*
     C                   call      'FA729R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    #2err
    C*
     C                   if        #2err = '1'
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck1end
     C                   endif
    C**********           ENDIF                           *IN99 = *OFFSYDT
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IN CHANGE MODE IF THE
    C* CORRESPONDING GROUP IS EXPIRED.  IF ONE OF THESE OPTIONS IS
    C* PRESSED, THEN GIVE THE USER THE EXPIRED GROUP WINDOW AND TAKE
    C* THEM INTO THEIR SELECTED OPTION IN DISPLAY MODE.
    C*          OPTIONS 2 AND 18
    C*
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   if        scsel = '2'
     C                             or scsel = '18'
     C*** BEG ADD *** 12/12/05 *********************************************
     C                             or scsel = '9'
     C*** END ADD *** 12/12/05 *********************************************
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** FNKY      CHAINPFN02                99
    C********** *IN99     IFEQ *OFF
    C**********           TIME           DSDTTM
    C**********           Z-ADDDSDATE    SRDTI
    C**********           MOVE *BLANKS   SRDTO
    C**********           MOVE *BLANKS   SRERR
    C**********           CALL 'SRCVTDAT'DTCYMD
    C**********           MOVELSRDTO     HLSYDT
    C********** FNEXDT    IFNE *ZEROS
    C********** FNEXDT    ANDLTHLSYDT
    C*
     C                   call      'FA729R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    #2err
    C*
     C                   if        #2err = '1'
     C                   call      'FA720R'
     C                   parm      '2'           ##mgid
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
    C*
     C                   if        scsel = '2'
     C                   movel(p)  '5'           scsel
     C                   endif
    C*
     C                   move      'D'           ##mode
     C                   goto      ck1end
     C                   endif
    C*
    C*************        ENDIF                           *IN99 = *OFF
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IF THE
    C* CLAIM INDICATES A 'Y' IN THE DENIED FLAG AND THE USER'S
    C* SECURITY LEVEL AND TYPE DOES NOT PASS THE SECURTY LEVEL AND
    C* TYPE DEFINED INTHE CLAIMS/DARTS CONTROL FILE.
    C*          OPTIONS 9 AND 19
    C*
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   if        scsel = '9'
     C                             or scsel = '19'
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   if        cmdny = 'Y'
     C                   eval      ##dnsl = 0
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
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           MOVELQ1USER    KKUSR     P
    C********** UPKY      CHAINPUP02                99
    C********** *IN99     IFEQ *OFF
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
    C*** END   CHG *** 06/01/01 **************************************
    C*****      UPLVL     IFLT ##DNSL
    C*****      UPTYPE    ORNE ##DNST
     C                   call      'SA006R'
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           PARM           UPLVL
    C**********           PARM           UPTYPE
     C                   parm                    ##lvl
     C                   parm                    ##type
    C*** END   CHG *** 06/01/01 **************************************
     C                   parm                    ##dnsl
     C                   parm                    ##dnst
     C                   parm      ' '           ##rtcd
     C                   if        ##rtcd = 'F'
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck1end
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   endif
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C* THE FOLLOWING OPTIONS ARE NOT ALLOWED IN CHANGE MODE IF THE
    C* CLAIM INDICATES A 'Y' IN THE DENIED FLAG AND THE USER'S
    C* SECURITY LEVEL AND TYPE DOES NOT PASS THE SECURTY LEVEL AND
    C* TYPE DEFINED INTHE CLAIMS/DARTS CONTROL FILE.
    C*          OPTIONS 2, 17 AND 18
    C*
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   if        scsel = '2'
     C                             or scsel = '18'
     C*** BEG ADD *** 12/12/05 *********************************************
     C                             or scsel = '9'
     C*** END ADD *** 12/12/05 *********************************************
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
    C*
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   if        cmdny = 'Y'
     C                   eval      ##dnsl = 0
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
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           MOVELQ1USER    KKUSR     P
    C********** UPKY      CHAINPUP02                99
    C********** *IN99     IFEQ *OFF
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
    C*** END   CHG *** 06/01/01 **************************************
    C********** UPLVL     IFLT ##DNSL
    C********** UPTYPE    ORNE ##DNST
     C                   if        ##lvl < ##dnsl
     C                             or ##type <> ##dnst
    C*** END   CHG *** 06/01/01 **************************************
    C*
     C                   call      'FA720R'
     C                   parm      '3'           ##mgid
     C                   parm                    ##co#
     C                   parm                    scsub#
     C                   parm                    scfnd
    C*
     C                   if        scsel = '2'
     C                   movel(p)  '5'           scsel
     C                   endif
    C*
     C                   move      'D'           ##mode
     C                   goto      ck1end
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   endif
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*
    C*** BEG ADD *** 05/07/99 ****************************************
    C**********
    C**CONDITION ABILITY TO A RECON ON THE RECON ALLOWANCE FLAG
    C**RECON*SECURITY LEVEL AND TYPE
    C**********
    C********** SCSEL     IFEQ '2'
    C********** SCBLST    ANDEQ'C'
    C**********           Z-ADDSCSUB#    KKSUB#
    C**********           Z-ADDSCFND     KKFND
    C**********           Z-ADDSCFYR     KKFYR
    C**********           Z-ADDSCCASE    KKCASE
    C**********           Z-ADDSCEOB#    KKEOB#
    C**********           MOVE *HIVAL    KKRCN#
    C********** EAKY1     SETGTPEA01
    C********** EAKY2     REDPEPEA01                    99
    C********** *IN99     IFEQ *OFF
    C********** EAEPFL    ANDEQ'P'
    C**********           CALL 'FA715R'
    C**********           PARM ' '       ##RTCD
    C**********           PARM           ##CO#
    C**********           PARM           SCSUB#
    C**********           PARM           SCFND
    C**********           PARM           ##PVMC  10
    C**********           PARM           ##PVPR  10
    C**********           PARM           ##WCUP  1
    C**********           PARM           ##CLDL  30
    C**********           PARM           ##CLDT  1
    C**********           PARM           ##CLTL  30
    C**********           PARM           ##CLTT  1
    C**********           PARM           ##SDCL  30
    C**********           PARM           ##SDCT  1
    C**********           PARM           ##BSCL  30
    C**********           PARM           ##BSCT  1
    C**********           PARM           ##DCDL  30
    C**********           PARM           ##DCDT  1
    C**********           PARM           ##DCCL  30
    C**********           PARM           ##DCCT  1
    C**********           PARM           ##C#CL  30
    C**********           PARM           ##C#CT  1
    C**********           PARM           ##FIMR  5
    C**********           PARM           ##TIMR  5
    C**********           PARM           ##FRN   5
    C**********           PARM           ##TRN   5
    C**********           PARM           ##CS#L  30
    C**********           PARM           ##CS#T  1
    C**********           PARM           ##DNSL  30
    C**********           PARM ' '       ##DNST  1
    C**********           PARM           ##NERC  5
    C**********           PARM           ##MEUS  10
    C**********           PARM           ##NWPA  5
    C**********           PARM           ##CPAA  5
    C**********           PARM           ##CPAD  5
    C**********           PARM           ##PLCL  30
    C**********           PARM           ##PLCT  1
    C**********           PARM           ##CKDL  30
    C**********           PARM           ##CKDT  1
    C**********           PARM ' '       ##RCAL  1
    C**********           PARM *ZEROS    ##RCSL  30
    C**********           PARM ' '       ##RCST  1
    C**********
    C********** ##RCAL    IFEQ 'N'
    C**********           SETON                     9087
    C**********           GOTO CK1END
    C**********           ELSE
    C**********           MOVELQ1USER    KKUSR     P
    C********** UPKY      CHAINPUP02                99
    C********** *IN99     IFEQ *OFF
    C********** UPLVL     IFLT ##RCSL
    C********** UPTYPE    ORNE ##RCST
    C**********           SETON                     9088
    C**********           GOTO CK1END
    C**********           ENDIF                           UPLBL LT ##RCSL
    C**********           ENDIF                           *IN99 = *OFF
    C**********
    C**********           ENDIF                           ##RCAL = 'N'
    C**********
    C**********           ENDIF                           *IN99 = *OFF
    C**********
    C**********           ENDIF                           SCSEL = '2'
    C*** END ADD *** 05/07/99 ****************************************
    C*** BEGIN CHG *** 07/29/99 **************************************
    C*
    C*          Option 3 = Copy
    C*             Call F7098 to perform copies
    C*          ------------------------------------------------------
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   if        scsel = '3'
     C                   call      'F7098R'
     C                   parm      scdbrr        @3rrn
     C                   parm                    @3rrn2
     C                   parm                    @3rrn3
    C*
    C*             Call F0395 to display new EOMB
    C*             ---------------------------------------------------
     C                   move      'D'           @3mode
     C                   move      'P'           @3blst
     C                   call      'F0395R01'
 P1  C                   parm      *inkc         @3kc
 P2  C                   parm      *inkl         @3kl
 P3  C                   parm                    @3mode
 P4  C                   parm                    @3rrn2
 P5  C                   parm                    @3rrn3
 P6  C                   parm                    @3blst
 P7  C                   parm      *blanks       @3stcd
     C                   parm      *blanks       @3reas
     C                   parm      *blanks       @3pgm
     C                   clear                   scsel
     C                   eval      *in90 = *on
     C                   endif
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*** END CHG *** 07/29/99 ****************************************
    C*
    C*** BEG ADD *** 09/10/99 ****************************************
    C*
    C* DETERMINE IF THE OPTION2=CHANGE IS A REGULAR CHANGE OR TAPE
    C* SUBMISSION CORRECTION OR EDI CORRECTION AND TAKE THE
    C* APPROPRIATE PATH
    C*
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   if        scsel = '2'
     C     scdbrr        chain     pfrcdf                             99
     C                   if        *in99 = *off
     C                             and eoblst = 'C'
     C                   if        eotape = 'Y'
    C*** BEGIN ADD *** 02/11/02 **************************************
     C                             or eotape = 'R'
    C*** END   ADD *** 02/11/02 **************************************
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   eval      kkrcn = eorcn
    C*** BEG CHG *** 03/08/00 ****************************************
    C**********           MOVE *HIVAL    KKLSEQ
    C********** LGKY1     SETGTPLG01
    C********** LGKY2     REDPEPLG01                    99
    C********** *IN99     IFEQ *OFF
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
    C*
     C                   if        ##sts <> '  '
    C********** LGSTS     IFEQ 'SN'
     C                   if        ##sts = 'SN'
     C                   eval      *in90 = *on
     C                   eval      *in87 = *on
     C                   goto      ck1end
     C                   endif
    C********** LGSTS     IFEQ 'TA'
     C                   if        ##sts = 'TA'
     C                   move      'R'           ##nwst
     C                   else
     C                   move      'E'           ##nwst
     C                   endif
     C                   else
     C                   if        eocntl = *blanks
     C                   move      'R'           ##nwst
     C                   else
     C                   move      'B'           ##nwst
     C                   endif
     C                   endif
     C                   else
     C                   move      'R'           ##nwst
     C                   endif
    C***********          ENDIF                           *IN99 = *OFF
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
     C                   endif
    C*** END ADD *** 09/10/99 ****************************************
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*** BEGIN ADD *** 05/23/02 **************************************
    C*
    C* DON'T ALLOW A TWO TO CHANGE IF CODE REVIEW PENDED THIS ERROR
    C* DUE TO AN ERROR IN PROCESSING.
    C*
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   if        *in90 = *off
     C*** END   ADD *** 06/14/02 *******************************************
     C                   if        scsel = '2'
     C                             and sccrpe = 'E'
    C*
     C                   exsr      mvdb04
     C                   exfmt     scr04
     C                   seton                                        9084
    C*
     C                   endif
    C*** END   ADD *** 05/23/02 **************************************
     C*** BEGIN ADD *** 06/14/02 *******************************************
     C                   endif
     C*** END   ADD *** 06/14/02 *******************************************
    C*** BEGIN ADD *** 10/29/02 **************************************
     C                   endsl
    C*** END   ADD *** 10/29/02 **************************************
    C*
     C     ck1end        endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
    C*
    C* PROCESS SUBSET SEARCHES
    C*
     C                   exsr      othfks
     C     *in90         cabeq     *on           ck2end
    C*
    C* CLEAR SUBSET DESCRIPTION FIELDS
    C*
     C                   exsr      sscler
    C*
    C* START OF ERROR CHECKING
    C*
     C                   if        ssfnd <> *zeros
     C                             and sssub# = *zeros
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        sssub# = *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        ssfnd = *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        ssfyr = *zeros
     C                             and sscase <> *zeros
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   if        ssfein = *zeros
     C                             and ssmod <> *blanks
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck2end
     C                   endif
    C*
    C* validate subscriber number
    C*
     C                   if        sssub# <> *zeros
     C                   eval      kksub# = sssub#
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** SMKY      CHAINPSM02                99
    C********** *IN99     IFEQ *ON
    C**********           SETON                     9075
    C**********           GOTO CK2END
    C**********           ENDIF                           *IN99 = *ON
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
     C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in75 = *on
     C                   goto      ck2end
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
     C                   endif
    C*
    C* validate group
    C*
     C                   if        ssfnd <> *zeros
     C                   eval      kkfnd = ssfnd
     C                   eval      kksub# = sssub#
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** FNKY      CHAINPFN02                99
    C********** *IN99     IFEQ *ON
    C**********           SETON                     9076
    C**********           GOTO CK2END
    C**********           ENDIF                           *IN99 = *ON
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm                    #2err
    C*
     C                   if        #2err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in76 = *on
     C                   goto      ck2end
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
     C                   endif
    C*
    C* validate case
    C*
     C                   if        sscase <> *zeros
     C                   eval      kksub# = sssub#
     C                   eval      kkfnd = ssfnd
     C                   eval      kkfyr = ssfyr
     C                   eval      kkcase = sscase
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *on
     C                   eval      *in90 = *on
     C                   eval      *in77 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* validate provider
    C*
     C                   if        ssfein <> *zeros
     C                             and ssmod <> *blanks
     C                   eval      kkfein = ssfein
     C                   movel     ssmod         kkmod
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** PDKY      CHAINPPD01                99
    C********** *IN99     IFEQ *ON
    C**********           SETON                     9078
    C**********           GOTO CK2END
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck2end
    C*** END CHG *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 04/13/99 **************************************
     C                   else
    C*
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           MOVE SSMOD     SXMOD
    C*** END   DEL *** 04/27/99 **************************************
    C*** END   ADD *** 04/13/99 **************************************
     C                   endif
     C                   endif
    C*
     C                   if        ssfein <> *zeros
     C                             and ssmod = *blanks
     C                   eval      kkfein = ssfein
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** PDKY1     CHAINPPD01                99
    C********** *IN99     IFEQ *ON
    C**********           SETON                     9078
    C**********           GOTO CK2END
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm                    #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck2end
    C*** END CHG *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 04/13/99 **************************************
     C                   else
    C*
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           Z-ADD*ZEROS    SXMOD
    C*** END   DEL *** 04/27/99 **************************************
    C*** END   ADD *** 04/13/99 **************************************
     C                   endif
     C                   endif
    C*
    C* validate dates of service
    C*
     C                   if        ssfrdt <> *zeros
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssfrdt
     C                   if        srerr = '1'
     C                   eval      *in90 = *on
     C                   eval      *in79 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
     C                   if        sstodt <> *zeros
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sstodt
     C                   if        srerr = '1'
     C                   eval      *in90 = *on
     C                   eval      *in83 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* From Service Date cannot be greater than To Service Date
    C*
     C                   if        ssfrdt > *zeros
     C                             and sstodt > *zeros
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlfrdt
    C*
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hltodt
    C*
     C                   if        hlfrdt > hltodt
     C                   eval      *in90 = *on
     C                   eval      *in84 = *on
     C                   goto      ck2end
     C                   endif
    C*
     C                   endif
    C*
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
    C* validate bill type
    C*
     C                   if        ssbtyp <> *blanks
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
     C                   if        ##err = '0'
     C                   eval      *in90 = *on
     C                   eval      *in89 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
    C* validate initial user
    C*
     C                   if        ssiusr <> *blanks
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           MOVELSSIUSR    KKUSR     P
    C********** UPKY      CHAINPUP02                99
    C********** *IN99     IFEQ *ON
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      *zeros        ##rrn
     C                   parm      ssiusr        ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = 'F'
    C*** END   CHG *** 06/01/01 **************************************
     C                   eval      *in90 = *on
     C                   eval      *in81 = *on
     C                   goto      ck2end
     C                   endif
     C                   endif
    C*
    C* CALL DATE FORMATTING SUBROUTINE
    C*
     C                   exsr      datsbr
    C*
     C     ck2end        endsr
    C*** BEGIN ADD *** 10/29/02 **************************************
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
     C                   dow       *in60 = *off
    C*
     C                   call      'SA008R'      rstplf
    C*
     C                   if        ##rtcd = *off
     C                   move      fnfnd         grp(x)
     C                   eval      x = x + 1
     C                   else
     C                   move      *on           grperr
     C                   endif
    C*
     C     fnky3         reade     pfn02                                  60
    C*
     C                   enddo
    C*
     C                   if        grperr = *on
    C*
     C                   cat       fnd:1         qryslt
    C*
     C                   do        x             z
     C                   cat       grp(z):1      qryslt
     C                   enddo
    C*
     C                   cat       '))':0        qryslt
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================
    C* GETSUB - SUBROUTINE TO GET SUBSCRIBERS USER IS AUTHORIZED TO
    C*===============================================================
     C     getsub        begsr
    C*
     C     ##co#         chain     psm01                              60
    C*
     C                   move      *on           first
     C                   move      ##co#         xaco#
    C*
     C                   dow       *in60 = *off
    C*
     C                   call      'SA008R'      rstpls
    C*
     C                   if        ##rtcd = *off
    C*
     C                   eval      ##sub# = smsub#
     C                   eval      kksub# = smsub#
     C                   move      smsub#        xasub#
    C*
     C                   if        first = *on
     C                   move      *off          first
     C     co#           cat(p)    xaco#:1       qryslt
     C                   cat       co#2:0        qryslt
     C                   cat       sub#:0        qryslt
     C                   cat       xasub#:1      qryslt
     C                   cat       ')':0         qryslt
     C                   else
     C                   cat       '*OR':1       qryslt
     C                   cat       sub#:1        qryslt
     C                   cat       xasub#:1      qryslt
     C                   cat       ')':0         qryslt
     C                   endif
    C*
     C                   clear                   grp
    C*
     C                   if        ##mrgr = 'Y'
     C                   exsr      getgrp
     C                   endif
    C*
     C                   endif
    C*
     C     ##co#         reade     psm01                                  60
    C*
     C                   enddo
    C*
     C                   cat       ')':0         qryslt
    C*
    C*  CALL CL TO CREATE AN OPEN QUERY FILE OVER THE FILES
    C*
     C     ovrdb1        cat(p)    'FAEOL69':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      31            length
    C*
     C     ovrdb1        cat(p)    'FAEOL70':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      31            length
    C*
     C     ovrdb1        cat(p)    'FAEOL71':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      31            length
    C*
     C     ovrdb1        cat(p)    'FAEOL72':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      32            length
    C*
     C     ovrdb1        cat(p)    'FAEOL73':0   xxovrd
     C                   cat       ovrdb2:0      xxovrd
    C*
     C                   call      'QCMDEXC'
     C                   parm      xxovrd        cmdstr
     C                   parm      44            length
    C*
     C                   call      qrypgm        qrypl                  99
    C*
     C                   endsr
    C*** END   ADD *** 10/29/02 **************************************
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   eval      #2sub# = 0
     C                   eval      #2fnd = 0
     C                   eval      #2fyr = 0
     C                   eval      #2case = 0
     C                   if        q1prms > 5
     C                             and @@sub# <> *zeros
     C                             and @@fnd <> *zeros
     C                             and @@fyr <> *zeros
     C                             and @@case <> *zeros
     C                   eval      sssub# = @@sub#
     C                   eval      ssfnd = @@fnd
     C                   eval      ssfyr = @@fyr
     C                   eval      sscase = @@case
    C*** BEGIN ADD *** 02/23/99 **************************************
    C*
    C* PROTECT THE SUBSET FIELDS FOR SUB#/FND/FYR/CASE
    C*
     C                   eval      *in50 = *on
    C*** END   ADD *** 02/23/99 **************************************
    C*
    C* SET CASE PARMS FOR CALL TO ADJPGM WHEN SUBSETTED ON CASE
    C*
     C                   eval      #2sub# = sssub#
     C                   eval      #2fnd = ssfnd
     C                   eval      #2fyr = ssfyr
     C                   eval      #2case = sscase
    C*** BEGIN ADD *** 02/23/99 **************************************
     C                   else
    C*
     C                   eval      *in50 = *off
    C*** END   ADD *** 02/23/99 **************************************
    C*
     C                   endif
    C*** BEGIN ADD *** 12/27/98 **************************************
    C*
    C* LOAD EOMB# AND PRE-CERT# INTO SUBSET FIELDS IF THE NUMBER OF
    C* PARMS BEING PASSED IS GREATER THAN 5.
    C*
     C                   if        q1prms > 5
    C*
    C* LOAD EOMB# INFORMATION INTO SSEOB# AND PROTECT IF NOT BLANK.
    C*
     C                   eval      sseob# = @@eob#
    C*
     C                   if        sseob# <> *zeros
    C*
     C                   eval      *in16 = *on
    C*
     C                   endif
    C*
    C* LOAD PRE-CERT INFORMATION INTO SSCERT AND PROTECT IF NOT BLANK.
    C*
     C                   if        q1prms > 6
    C*
     C                   movel(p)  @@cert        sscert
    C*
     C                   if        sscert <> *blanks
    C*
     C                   eval      *in17 = *on
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*** END   ADD *** 12/27/98 **************************************
    C*** BEGIN ADD *** 01/11/99 **************************************
     C                   if        q1prms >= 8
     C                   move      @@sfdt        ssfrdt
     C                   move      @@stdt        sstodt
     C                   move      @@sfei        ssfein
     C                   move      @@smod        ssmod
     C                   if        ssfein = *zeros
     C                             or ssmod = '000'
     C                   move      '   '         ssmod
     C                   endif
     C                   if        ssfrdt <> *zeros
     C                             or sstodt <> *zeros
     C                   eval      *in18 = *on
     C                   endif
     C                   if        ssfein <> *zeros
     C                             or ssmod <> *blanks
    C*** BEGIN DEL *** 01/02/02 **************************************
    C**********           SETON                     19
    C*** END   DEL *** 01/02/02 **************************************
     C                   endif
     C                   endif
    C*** END ADD *** 01/11/99 ****************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
     C*
     C* Check restriction flags and subset and protect.
     C*
     C                   movel(p)  q1user        kkusr
     C     upky          chain     pup01                              99
     C                   if        *in99=*off
     C                   select
    C*** BEGIN CHG *** 10/29/02 **************************************
     C**********         when      upmrgr='Y'
     C                   when      upmrgr = 'Y'
     C                             and ##rcds = *off
     C                             and upsub# <> *zeros
     C                             and upfnd <> *zeros
     C                             or *in55 = *off
     C                             and upmrgr = 'Y'
     C                             and upsub# <> *zeros
     C                             and upfnd <> *zeros
    C*** END   CHG *** 10/29/02 **************************************
     C                   eval      sssub#=upsub#
     C                   eval      ssfnd=upfnd
     C                   seton                                        5152
    C*** BEGIN CHG *** 10/29/02 **************************************
     C**********         when      upmrsu='Y'
     C                   when      upmrsu = 'Y'
     C                             and ##rcds = *off
     C                             and upsub# <> *zeros
     C                             or *in55 = *off
     C                             and upmrsu = 'Y'
     C                             and upsub# <> *zeros
    C*** END   CHG *** 10/29/02 **************************************
     C                   eval      sssub#=upsub#
     C                   seton                                        51
     C                   endsl
     C                   endif
     C*
    C*** END   ADD *** 05/30/02 **************************************
    C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
    C*** BEGIN ADD *** 10/29/02 **************************************
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
    C*** BEG ADD *** 08/08/05 *********************************************
12   C                   parm      *blank        ##kcif
12   C                   parm      *blank        ##curs
12   C                   parm      *blank        ##pcib
12   C                   parm      *blank        ##clt#
    C*** END ADD *** 08/08/05 *********************************************
    C*
     C                   eval      *in55 = *off
     C                   if        ##mgri = 'Y'
     C                   eval      *in55 = *on
     C                   endif
    C*** END   ADD *** 10/29/02 **************************************
     C                   move      *loval        kklsn
     C                   move      *loval        kkfsn
     C                   move      *loval        kkeob#
     C                   move      *loval        kkfein
     C                   move      *loval        kkmod
     C                   move      *loval        kkss#
     C                   move      *loval        kkacdt
    C*** BEGIN ADD *** 06/01/01 **************************************
     C                   move      *loval        kkcntl
    C*** END   ADD *** 06/01/01 **************************************
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   eval      kkfrdt = *hival
    C*** END   ADD *** 09/07/01 **************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
    C*
    C* VIEW# 1 (Last Name and First Name)
    C*
     C                   if        *in21 = *on
     C                   movel(p)  scpst1        kklsn
     C                   movel(p)  scpsta        kkfsn
     C                   endif
    C*** BEGIN ADD *** 09/07/01 **************************************
    C*** BEGIN ADD *** 12/10/01 **************************************
     C                   if        scpstf <> *zeros
     C                             and *in21 = *on
    C*** END   ADD *** 12/10/01 **************************************
     C                   if        *in21 = *on
     C                   move      scpstf        srdti
     C                   endif
     C                   if        *in21 = *on
     C                   call      'SRCVTDAT'    dtcymd
     C                   endif
     C                   if        *in21 = *on
     C                   movel     srdto         kkfrdt
     C                   endif
    C*** BEGIN ADD *** 12/10/01 **************************************
     C                   else
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkfrdt
     C                   endif
    C*** END   ADD *** 12/10/01 **************************************
     C                   if        *in21 = *on
     C                   eval      kkeob# = *zeros
     C                   endif
    C*** END   ADD *** 09/07/01 **************************************
    C*
    C* VIEW# 2 (Provider FEIN and MOD)
    C*
     C                   if        *in22 = *on
     C                   movel     scpst2        kkfein
     C                   move      scpstb        kkmod
     C                   endif
    C*
    C* VIEW# 3 (SS# and DOI)
    C*
     C                   if        *in23 = *on
     C                   movel     scpst3        kkss#
     C                   endif
     C                   if        *in23 = *on
     C                   movel     scpstc        srdti
     C                   clear                   srdto
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkacdt
     C                   endif
    C*
    C* VIEW# 4 (EOMB#)
    C*
     C                   if        *in24 = *on
     C                   move      scpst4        kkeob#
     C                   endif
    C*
    C*** BEG ADD *** 03/08/00 ****************************************
    C*
    C* VIEW# 5 (Control Number)
    C*
     C                   if        *in25 = *on
     C                   movel(p)  scpst5        kkcntl
     C                   endif
    C*
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
    C*****
    C***21                MOVELSCLSN     KKLSN     P
    C***21                MOVELSCFSN     KKFSN     P
    C***21                MOVELHLPNAM    KKPNAM    P
    C***22                MOVELSCFEIN    KKFEIN
    C***22                MOVELSCMOD     KKMOD
    C***23                MOVELSCSS#     KKSS#     P
    C*****      *IN23     IFEQ *ON
    C*****                MOVELSCACDT    SRDTI     P
    C*****                MOVE *BLANKS   SRDTO
    C*****                MOVE *BLANKS   SRERR
    C*****                CALL 'SRCVTDAT'DTCYMD
    C*****                MOVELSRDTO     KKACDT
    C*****                ENDIF                           *IN23 = *ON
    C***24                Z-ADDSCEOB#    KKEOB#
    C*
     C                   movel(p)  sclsn         kklsn
     C                   movel(p)  scfsn         kkfsn
     C                   movel(p)  hlpnam        kkpnam
     C                   movel     scfein        kkfein
     C                   movel     scmod         kkmod
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                movel(p)  scss#         kkss#
      /Free
       kkss# = UnMaskData('SSN':'kkss#':scSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
    C*****                MOVE SCACDT    SRDTI     P
    C*****                MOVE *BLANKS   SRDTO
    C*****                MOVE *BLANKS   SRERR
    C*****                CALL 'SRCVTDAT'DTCYMD
    C*****                MOVELSRDTO     KKACDT
     C                   eval      kkacdt = hlsdoi
     C                   eval      kkeob# = sceob#
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   movel(p)  sccntl        kkcntl
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   eval      kkfrdt = scfrd
    C*** END   ADD *** 09/07/01 **************************************
     C                   endsr
    C*================================================================
    C* KJDO - F10 Search Subroutine
    C*================================================================
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
    C*
    C* Subscriber Search
    C*
     C                   if        hfld = 'SSSUB#'
     C*** BEGIN CHG *** 05/30/02 *******************************************
     C                             and *in50=*off
     C                             and *in51=*off
     C*** END   CHG *** 05/30/02 *******************************************
     C                   eval      hlsub# = sssub#
     C                   movel     sssmnm        hlsmnm
     C                   eval      ##rrn = 0
     C                   call      'F0055R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
    C*
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                             or ##rrn = *zeros
     C                   eval      sssub# = hlsub#
     C                   movel     hlsmnm        sssmnm
     C                   eval      *in90 = *on
     C                   eval      *in30 = *on
     C                   else
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** ##RRN     CHAINPSM01                99
    C**N99*****           Z-ADDSMSUB#    SSSUB#
    C**N99*****           MOVELSMNAME    SSSMNM    P
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm                    ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '1'           ##err
    C*
     C                   movel(p)  ##name        sssmnm
     C                   eval      sssub# = ##sub#
    C*** BEG CHG *** 03/08/00 ****************************************
     C                   eval      *in90 = *on
     C                   eval      *in31 = *on
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
    C* Group Search
    C*
     C                   if        hfld = 'SSFND'
     C*** BEGIN CHG *** 05/30/02 *******************************************
     C                             and *in50=*off
     C                             and *in52=*off
     C*** END   CHG *** 05/30/02 *******************************************
     C                   eval      ##rrn = 0
     C                   eval      hlfnd = ssfnd
     C                   movel     ssfnnm        hlfnnm
     C                   call      'F0060R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
    C*
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                             or ##rrn = *zeros
     C                   eval      ssfnd = hlfnd
     C                   movel     hlfnnm        ssfnnm
     C                   eval      *in90 = *on
     C                   eval      *in31 = *on
     C                   else
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** ##RRN     CHAINPFN01                99
    C**N99*****           Z-ADDFNFND     SSFND
    C**N99*****           MOVELFNNAME    SSFNNM    P
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm                    #2sub#
     C                   parm                    #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm      '1'           #2err
    C*
     C                   eval      ssfnd = #2fnd
     C                   movel(p)  #2name        ssfnnm
    C*** END CHG *** 03/08/00 ****************************************
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
    C* Fund Year/Case Search
    C*
     C                   if        hfld = 'SSFYR'
     C                             or hfld = 'SSCASE'
     C                   eval      hlfyr = ssfyr
     C                   eval      hlcase = sscase
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
     C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fyr = *zeros
     C                             or ##case = *zeros
     C                   eval      ssfyr = hlfyr
     C                   eval      sscase = hlcase
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   else
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
     C                   if        *in99 = *off
     C                   movel(p)  cmlsn         ##last
     C                   movel(p)  cmfsn         ##frst
     C                   move      '1'           ##term
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm                    ##term
     C                   movel(p)  ##last        ssname
     C                   endif
    C*** BEG CHG *** 03/08/00 ****************************************
    C*
    C* get subscriber name
    C*
    C********** SMKY      CHAINPSM02                99
    C********** *IN99     IFEQ *OFF
    C**********           MOVELSMNAME    SSSMNM    P
    C**********           ENDIF                           *IN99 = *OFF
    C*
     C                   call      'FA726R'
     C                   parm                    ##co#
     C                   parm      kksub#        ##sub#
     C                   parm                    ##name
     C                   parm                    ##rrn
     C                   parm      '0'           ##err
    C*
     C                   movel(p)  ##name        sssmnm
    C*
    C* get group name
    C*
    C********** FNKY      CHAINPFN02                99
    C********** *IN99     IFEQ *OFF
    C**********           MOVELFNNAME    SSFNNM    P
    C**********           ENDIF                           *IN99 = *OFF
    C*
     C                   call      'FA727R'
     C                   parm                    ##co#
     C                   parm      kksub#        #2sub#
     C                   parm      kkfnd         #2fnd
     C                   parm                    #2name
     C                   parm                    ##rrn
     C                   parm                    #2err
    C*
     C                   movel(p)  #2name        ssfnnm
    C*
    C*** END CHG *** 03/08/00 ****************************************
    C*
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
     C                   endif
     C                   exsr      datsbr
     C                   endif
    C*
    C* Provider Search
    C*
     C                   if        hfld = 'SSFEIN'
     C                             or hfld = 'SSMOD'
     C                   move      ssfein        hlfein
     C                   move      ssmod         hlmod
     C                   call      'F0032R'
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##fein
     C                   parm      *blanks       ##mod
     C                   parm      *blanks       ##rtcd
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fein = *blanks
    C*** BEG DEL *** 05/16/03 ****************************************
     C****************** move      hlfein        ssfein
     C****************** move      hlmod         ssmod
    C*** END DEL *** 05/16/03 ****************************************
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
     C                   else
     C                   move      ##fein        ssfein
     C                   move      ##mod         ssmod
     C                   eval      *in90 = *on
     C                   eval      *in34 = *on
    C*** BEG DEL *** 05/16/03 ****************************************
     C*****************  endif
    C*** END DEL *** 05/16/03 ****************************************
    C*
     C                   eval      kkfein = ssfein
     C                   movel     ssmod         kkmod
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** PDKY      CHAINPPD01                99
    C**N99*****           MOVELPDNAME    SSPDNM 20 P
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err <> '0'
     C                   movel(p)  #1name        sspdnm
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
    C*** BEG ADD *** 05/16/03 ****************************************
     C                   endif
    C*** END ADD *** 05/16/03 ****************************************
    C*
     C                   exsr      datsbr
     C                   endif
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C                   if        hfld = 'SSBTYP'
    C*
     C                   call      'F2380R'
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##kc
     C                   parm      *blanks       ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      'BTYP'        ##tbid
    C*
     C                   if        ##kc = '1'
     C                             or ##kl = '1'
     C                             or ##rrn = *zeros
     C                   eval      *in90 = *on
     C                   eval      *in37 = *on
     C                   else
     C                   movel     hlbsds        ssbsds
     C                   eval      *in90 = *on
     C                   eval      *in36 = *on
    C*
     C                   call      'FA728R'
     C                   parm                    ##co#
     C                   parm                    ##tbid
     C                   parm      *blanks       #1key
     C                   parm      *blanks       ##desc
     C                   parm                    ##rrn
     C                   parm      '1'           ##err
    C*
     C                   movel     #1key         ssbtyp
     C                   movel     ##desc        scbtds
    C*
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
    C* Initial User Search
    C*
     C                   if        hfld = 'SSIUSR'
     C                   movel(p)  ssiusr        hliusr
     C                   movel(p)  ssusnm        hlusnm
     C                   call      'S0005R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm      *zeros        ##view
     C                   if        ##rrn = *zeros
     C                   movel(p)  hliusr        ssiusr
     C                   movel(p)  hlusnm        ssusnm
     C                   eval      *in90 = *on
     C                   eval      *in36 = *on
     C                   else
    C*** BEGIN CHG *** 06/01/01 **************************************
    C********** ##RRN     CHAINPUP01                99
    C**N99*****           MOVELUPUSR     SSIUSR    P
    C**N99*****           MOVELUPUSNM    SSUSNM    P
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      ##rrn         ##rrn
     C                   parm      *blanks       ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = *blanks
     C                   movel(p)  ##user        ssiusr
     C                   movel(p)  ##usnm        ssusnm
     C                   else
     C                   clear                   ssiusr
     C                   clear                   ssusnm
     C                   endif
    C*** END   CHG *** 06/01/01 **************************************
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   exsr      datsbr
     C                   endif
    C*
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*================================================================
    C* KNDO - DARTS Subroutine
    C*================================================================
     C     kndo          begsr
     C                   endsr
    C*** BEGIN ADD *** 01/11/99 **************************************
    C*================================================================
    C* OPT6 - Detail Charge List
    C*================================================================
     C     opt6          begsr
     C                   move      scacdt        x1dt
     C                   move      x1dtm         x2dtm
     C                   move      x1dtd         x2dtd
     C                   move      x1dty         x2dty
     C                   move      x2dt          srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         qqacdt
     C                   move      scmod         qqmod
     C                   call      'F9951R'
     C                   parm                    scprds
     C                   parm      ' '           ##call
     C                   parm                    scsub#
     C                   parm                    scfnd
     C                   parm                    scss#
     C                   parm                    qqacdt
     C                   parm                    scfein
     C                   parm                    qqmod
    C*** BEGIN ADD *** 05/24/01 **************************************
     C                   parm      'Y'           qqmode
    C*** END   ADD *** 05/24/01 **************************************
     C                   endsr
    C*** END ADD *** 01/11/99 ****************************************
    C*================================================================
    C* OPT7 - Work with Detail
    C*================================================================
     C     opt7          begsr
    C*
     C                   move      'D'           ##mode
    C*
     C                   exsr      ovride
     C                   move      ' '           ##kl
    C**********           MOVEL'F0353R'  ##PGM  10
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** BEKY      CHAINPBE01                99
    C********** *IN99     IFEQ *OFF
    C**********           MOVELBEDEPG    ##PGM     P
    C**********           ENDIF                           *IN99 = *OFF
    C*
     C                   call      'FA730R'
     C                   parm      scast         ##ast
     C                   parm      scsys         ##sys
     C                   parm      scbtyp        ##btyp
     C                   parm                    ##depg
    C*
     C                   move(p)   ##depg        ##pgm
    C*** END CHG *** 03/08/00 ****************************************
    C*
     C                   clear                   ##pgm2
    C*
     C                   call      'F0390R'
     C                   parm                    ##mode
     C                   parm                    ##kl
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##pgm
     C                   parm                    ##pgm2
     C                   exsr      dlovrd
     C                   endsr
    C*================================================================
    C* OPT8 - Work with Amounts
    C*================================================================
     C     opt8          begsr
     C                   call      'F0396R'
     C                   parm      ' '           ##call
     C                   parm                    scdbrr
     C                   endsr
    C*================================================================
    C* OPT10 - Work with Bill Status History
    C*================================================================
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
     C                   if        *in99 = *off
     C                   eval      ##rrn = dbrrna
     C                   endif
     C                   call      'F0395R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm      scdbrr        ##rrn2
     C                   exsr      dlovrd
     C                   endsr
    C*================================================================
    C* OPT11 - Work with EOB Notes
    C*================================================================
     C     opt11         begsr
    C*
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   move      sceob#        kkeob#
    C*
     C     esky          chain     pes01                              99
     C                   if        *in99 = *off
     C                   eval      ##rrn = dbrrna
     C                   endif
     C                   call      'F0355R'
    C*** BEG CHG *** 06/07/00 ****************************************
    C***********          PARM ' '       ##CALL
     C                   parm      '1'           ##call
    C*** END CHG *** 06/07/00 ****************************************
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm      scdbrr        ##rrn2
     C                   endsr
    C*================================================================
    C* OPT12 - Print EOMB
    C*================================================================
     C     opt12         begsr
    C*
     C                   move      scdbrr        #1rrn
    C*** BEGIN CHG *** 06/01/01 **************************************
    C**********           MOVELQ1USER    KKUSR     P
    C********** UPKY      CHAINPUP02                99
    C********** *IN99     IFEQ *OFF
    C**********           MOVELUPOUTQ    ##QUE     P
     C                   call      'SA012R'
     C                   parm      q1user        ##user
     C                   parm      *blanks       ##outq
     C                   parm      *blanks       ##rtcd
    C*
     C                   if        ##rtcd = *blanks
    C*** BEGIN CHG *** 11/19/01 **************************************
    C**********           MOVEL##USER    ##QUE     P
     C                   movel(p)  ##outq        ##que
    C*** END   CHG *** 11/19/01 **************************************
    C*** END   CHG *** 06/01/01 **************************************
     C                   endif
     C                   call      'F0340C'
     C                   parm                    #1rrn
     C                   parm                    scast
     C                   parm      'P'           #1mode
     C                   parm                    ##que
    C*
     C                   endsr
    C*================================================================
    C* OPT13 - Display EOMB
    C*================================================================
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
    C*
     C                   endsr
    C*================================================================
    C* OPT14 - DARTS
    C*================================================================
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
    C*================================================================
    C* OPT15 - Pre-Cert Info
    C*================================================================
     C     opt15         begsr
    C*
    C*
     C                   move      scsub#        kksub#
     C                   move      scfnd         kkfnd
     C                   move      scfyr         kkfyr
     C                   move      sccase        kkcase
     C                   movel     sccert        kkcert
     C                   eval      kkext = scext
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** PCKY      CHAINPPC01                99
    C********** *IN99     IFEQ *OFF
    C**********           Z-ADDDBRRNB    ##RRN
    C**********           ELSE
    C**********           Z-ADD0         ##RRN
    C**********           ENDIF                           *IN99 = *OFF
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
    C*** END CHG *** 03/08/00 ****************************************
    C*
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
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
     C                   endsr
    C*** BEGIN ADD *** 01/11/99 **************************************
    C*** BEGIN ADD *** 04/08/99 **************************************
    C*================================================================
    C* OPT16 - History
    C*================================================================
     C     opt16         begsr
    C*** BEG DEL *** 12/29/00 ****************************************
    C***********          MOVE SHACDT    Q8ACDT  80
    C***********          CALL 'F0575R'
P1  C***********          PARM           SCPRDS
P2  C***********          PARM ' '       ##CALL
P3  C***********          PARM           SCFYR
P4  C***********          PARM           SCCASE
P5  C***********          PARM           SCSS#
P6  C***********          PARM           Q8ACDT
P7  C***********          PARM ' '       ##RTCD
P8  C***********          PARM           ##CO#
P9  C***********          PARM           SCSUB#
P10 C***********          PARM           SCFND
    C*** END DEL *** 12/29/00 ****************************************
    C*** BEG ADD *** 12/29/00 ****************************************
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
    C*** END ADD *** 12/29/00 ****************************************
     C                   endsr
    C*** END ADD *** 04/08/99 ****************************************
    C*================================================================
    C* OPT20 - Option 20 (Pended Lines)
    C*================================================================
     C     opt20         begsr
    C*
    C*  Call new pended lines pgm F0398R
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
    C*** BEG ADD *** 10/13/00 ****************************************
    C*================================================================
    C* OPT22 - View Prov Info
    C*================================================================
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
     C                   eval      *in90 = *on
     C                   endsr
    C*** END ADD *** 10/13/00 ****************************************
    C*** BEGIN ADD *** 06/24/99 **************************************
    C*================================================================
    C* OPT30 - Letters
    C*================================================================
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
    C*** END   ADD *** 06/24/99 **************************************
    C*** BEGIN ADD *** 04/08/99 **************************************
    C*================================================================
    C* OPT81 - Claim info
    C*================================================================
     C     opt81         begsr
     C                   move      ##co#         #1co#
     C                   move      scsub#        #1sub#
     C                   move      scfnd         #1fnd
     C                   call      'FA701R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       ##outs
     C                   if        ##rtcd = *blank
     C                             and ##outs <> 'Y'
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   if        ##rtcd = *blank
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
     C                   call      'WA004R'
     C                   parm                    #1co#
     C                   parm      #1wcf#        #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      *zeros        #2rrn
     C                   if        #2rrn <> 0
     C                   call      'W0070R'
     C                   parm      '1'           ##call
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'M'           ##mode
     C                   parm                    #2rrn
     C                   parm      *blanks       ##sort
     C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   else
     C                   eval      *in85 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endsr
    C*================================================================
    C* OPT82 - Case info
    C*================================================================
     C     opt82         begsr
     C                   eval      kksub# = scsub#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *off
     C                   call      'F0260R'
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'L'           ##mode
     C                   parm      dbrcm1        ##rrn
     C                   parm      *blanks       ##ss#
     C                   parm      *zeros        ##acdt
     C                   endif
     C                   endsr
    C*** END ADD *** 04/08/99 ****************************************
     C*** BEG ADD *** 12/12/05 *********************************************
    C*================================================================
    C* OPT9 - Complete the bill
    C*================================================================
     C     opt9          begsr
     C*
     C                   eval      ##rrn2 = scdbrr
     C                   eval      ##chrg = eochrg
     C                   call      'F0393R'
     C                   parm      ' '           ##kl
     C                   parm      'C'           #1mode
     C                   parm                    ##rrn2
     C                   parm      'C'           ##cpdf            1
     C                   parm      ' '           ##cppn            3
     C                   parm      ' '           ##canc            1
     C                   parm                    ##CHRG           11 2
     C*
     C                   endsr
     C*** END ADD *** 12/12/05 *********************************************
    C*================================================================
    C* OPT99 - Soft Delete of EOMB
    C*================================================================
     C     opt99         begsr
    C*
     C                   eval      scsf1r = sfrrn1
     C                   add       1             sfrrn2               14
     C                   write     scr02s
     C                   eval      *in08 = *on
     C                   eval      scrrn2 = 1
     C                   write     scr02
     C                   exfmt     scr02c
     C                   if        *inkl = *off
     C                   move      'G'           ##mode
    C*
    C*** CALL BILL STATUS HISTORY
    C*
     C                   eval      ##rrn = 0
     C                   eval      ##rrn2 = scdbrr
     C                   clear                   ##stcd
     C                   clear                   ##reas
     C                   movel(p)  q1pgm         #1pgm
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
     C                   if        ##kl = *off
     C                             and ##kc = *off
     C                   call      acdpgm        acdpl
     C                   endif
     C                   endif
    C*
     C                   eval      *in14 = *off
     C                   endsr
    C*================================================================
    C* OVRIDE - Overide files based  on adjudication state
    C*================================================================
     C     ovride        begsr
     C                   eval      length = 56
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(2)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C                   movel(p)  qca(3)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
    C*** BEG ADD *** 05/12/99 ****************************************
    C*
     C                   movel(p)  qca(4)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
    C*** BEG CHG *** 10/27/99 ****************************************
    C**********           MOVELQCA,5     QQCMD     P
    C**********           MOVELSCAST     QQST      P
    C**********           CALL 'QCMDEXC'
    C**********           PARM           QQCMD
    C**********           PARM           LENGTH
    C**********           MOVELQCA,6     QQCMD     P
    C**********           MOVELSCAST     QQST      P
    C**********           CALL 'QCMDEXC'
    C**********           PARM           QQCMD
    C**********           PARM           LENGTH
    C**********
     C                   movel(p)  qca(5)        qqcmd2
     C                   movel(p)  scast         qqst2
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd2
     C                   parm                    length
     C                   movel(p)  qca(6)        qqcmd2
     C                   movel(p)  scast         qqst2
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd2
     C                   parm                    length
    C*** END CHG *** 10/27/99 ****************************************
     C                   movel(p)  qca(7)        qqcmd
     C                   movel(p)  scast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
    C*** END ADD *** 05/12/99 ****************************************
     C                   endsr
    C*** BEGIN ADD *** 05/23/02 **************************************
    C*================================================================
    C* MVDB04 - Move data base fields to SCR04
    C*================================================================
     C     mvdb04        begsr
    C*
    C** Get standard screen headings
    C*
     C     scdbrr        chain     pfrcdf                             99
    C*
     C                   movel     eospcd        scspcd
     C                   movel     eontwk        scnwcd
     C                   movel     eoflic        scsvzp
     C*
     C                   setoff                                       69
     C                   if        scnwcd <> *blanks
     C                   seton                                        69
     C                   endif
    C*
     C                   movel     *hival        kkidt
     C                   movel     *hival        kkitim
     C     esky2         setgt     pes01
     C     esky          readpe    pes01                                  99
     C                   if        *in99 = *off
     C                   eval      screas = esreas
     C                   else
     C                   eval      screas = *blanks
     C                   endif
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
    C*** END   ADD *** 05/23/02 **************************************
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   if        vldopt = 'N'
    C*
     C                   exsr      assrrn
    C*
    C*** BEG DEL *** 11/19/99 ****************************************
    C**********           ELSE
    C**********
    C**********           MOVE 'N'       VLDOPT
    C*** END DEL *** 11/19/99 ****************************************
    C*
     C                   endif
    C*
    C*** BEGIN ADD *** 05/23/02 **************************************
     C                   movel     eocrpe        sccrpe
    C*** END   ADD *** 05/23/02 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   movel     eocntl        sccntl
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 09/07/01 **************************************
     C                   eval      scfrd = eofrdt
    C*** END   ADD *** 09/07/01 **************************************
     C                   movel     eoast         scast
     C                   movel     eosys         scsys
     C                   movel     eobtyp        scbtyp
     C                   movel(p)  eolsn         sclsn
     C                   movel(p)  eofsn         scfsn
     C                   movel(p)  sclsn         ##last
     C                   movel(p)  scfsn         ##frst
     C                   move      '1'           ##term
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm                    ##term
     C                   movel     ##last        scname
    C*
     C                   move      eoeob#        sceob#
    C*** BEGIN ADD *** 04/03/00 **************************************
     C                   eval      scrcn = eorcn
    C*** END   ADD *** 04/03/00 **************************************
     C                   movel(p)  eoeob#        qqnum
     C                   clear                   qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         aleob#
    C*
     C                   eval      kkfein = eofein
     C                   eval      kkmod = eomod
    C*** BEG CHG *** 03/08/00 ****************************************
    C********** PDKY      CHAINPPD01                99
    C**N99*****           MOVELPDNAME    SCPNAM    P
     C                   call      'FA725R'
     C                   parm      kkfein        #1fein
     C                   parm      kkmod         #1mod
     C                   parm                    #1name
     C                   parm                    #1err
    C*
     C                   if        #1err <> '0'
     C                   movel(p)  #1name        scpnam
     C                   endif
    C*** END CHG *** 03/08/00 ****************************************
    C*
    C** 'HLPNAM' FIELD IS USED TO HOLD ENTIRE PROVIDER NAME FOR
    C**  USE IN KKPNAM KEY FIELD FOR PAGING (KEYSFL SUBROUTINE)
    C*
    C*** BEG CHG *** 03/08/00 ****************************************
    C**********           MOVELPDNAME    HLPNAM
     C                   movel     #1name        hlpnam
    C*** END CHG *** 03/08/00 ****************************************
    C*
     C                   eval      srdti = eofrdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         scfrdt
    C*
     C                   eval      srdti = eotodt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         sctodt
    C*
     C                   eval      srdti = eoacdt
     C                   eval      hlsdoi = eoacdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         hldate
    C*** BEGIN ADD *** 04/08/99 **************************************
     C                   movel     srdto         shacdt
    C*** END ADD *** 04/08/99 ****************************************
     C                   movel(p)  hldate        qqnum
     C                   movel(p)  'DATE'        qqmask
     C                   call      'SA020R'      chgpl
     C                   movel     qqnum         scacdt
    C*
     C                   if        eoblst = 'D'
     C                   movel(p)  'DEL'         scblst
     C                   else
     C                   movel(p)  eoblst        scblst
     C                   endif
    C*
     C                   move      eofein        scfein
     C                   move      scfein        alfein
     C                   move      eomod         scmod
     C                   move      scmod         almod
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                movel(p)  eoss#         scss#
      /Free
       scSS# = MaskData('SSN':'eoss#':eoss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
    C*
     C                   movel(p)  eochrg        qqnum
     C                   movel(p)  'X,XXX.XX'    qqmask
     C                   call      'SA020R'
     C                   parm                    qqnum
     C                   parm                    qqmask
     C                   movel     qqnum         scchrg
     C                   move      scchrg        alchrg
    C*
     C                   movel(p)  eopaid        qqnum
     C                   movel(p)  'X,XXX.XX'    qqmask
     C                   call      'SA020R'
     C                   parm                    qqnum
     C                   parm                    qqmask
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
     C                   select
    C*** BEG CHG *** 03/08/00 ****************************************
    C*  I had to change the HD#s to accomidate the new Control
    C*  Number view.
    C*
    C*** LOAD VIEW# 1 FIELDS
    C*
     C                   when      hd# = 1
     C                             or hd# = 6
     C                             or hd# = 11
     C                             or hd# = 16
     C                   eval      y = 1
     C                   movea     scname        dta1(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta1(y)
     C                   eval      y = y + 11
    C*** BEGIN CHG *** 04/12/99 **************************************
    C********             MOVEASCPNAM    DTA1,Y
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta1(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta1(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta1(y)
    C*** END CHG *** 04/12/99 ****************************************
     C                   movea(p)  dta1          scdata
    C*
    C*** LOAD VIEW# 2 FIELDS
    C*
     C                   when      hd# = 2
     C                             or hd# = 7
     C                             or hd# = 12
     C                             or hd# = 17
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
     C                   when      hd# = 3
     C                             or hd# = 8
     C                             or hd# = 13
     C                             or hd# = 18
     C                   eval      y = 1
     C                   movea     scss#         dta3(y)
     C                   eval      y = y + 11
     C                   movea     scacdt        dta3(y)
     C                   eval      y = y + 11
     C                   movea     aleob#        dta3(y)
    C*** BEGIN CHG *** 04/12/99 **************************************
    C********             ADD  11        Y
     C                   eval      y = y + 10
    C********             MOVEASCPNAM    DTA3,Y
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta3(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta3(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta3(y)
    C*** END CHG *** 04/12/99 ****************************************
     C                   movea(p)  dta3          scdata
    C*
    C*** LOAD VIEW# 4 FIELDS
    C*
     C                   when      hd# = 4
     C                             or hd# = 9
     C                             or hd# = 14
     C                             or hd# = 19
     C                   eval      y = 1
     C                   movea     aleob#        dta4(y)
     C                   eval      y = y + 11
     C                   movea     scname        dta4(y)
     C                   eval      y = y + 21
    C*** BEGIN CHG *** 04/12/99 **************************************
    C********             MOVEASCPNAM    DTA4,Y
     C                   move      scfein        x1fein
     C                   movea     x1fein        dta4(y)
     C                   eval      y = y + 9
     C                   movea     ' '           dta4(y)
     C                   eval      y = y + 1
     C                   move      scmod         x1mod
     C                   movea     x1mod         dta4(y)
    C*** END CHG *** 04/12/99 ****************************************
     C                   movea(p)  dta4          scdata
    C*** END CHG *** 03/08/00 ****************************************
    C*** BEG ADD *** 03/08/00 ****************************************
    C*
    C*** LOAD VIEW# 5 FIELDS
    C*
     C                   when      hd# = 5
     C                             or hd# = 10
     C                             or hd# = 15
     C                             or hd# = 20
     C                   eval      y = 1
     C                   movea     eocntl        dta5(y)
     C                   eval      y = y + 15
     C                   movea     scname        dta5(y)
     C                   eval      y = y + 21
     C                   movea     aleob#        dta5(y)
     C                   movea(p)  dta5          scdata
    C*
    C*** END ADD *** 03/08/00 ****************************************
     C                   endsl
    C*
     C                   select
    C*
    C* LOAD INFO #1
    C*
     C                   when      info# = 1
    C*****                Z-ADD1         X       20
     C                   eval      x = 2
     C                   movea     scfrdt        inf1(x)
     C                   eval      x = x + 12
     C                   movea     sctodt        inf1(x)
    C*****                ADD  14        X
     C                   eval      x = x + 13
     C                   movea     scblst        inf1(x)
     C                   movea     inf1          hlinf1
     C                   move      hlinf1        scdata
    C*
    C* LOAD INFO #2
    C*
     C                   when      info# = 2
     C                   eval      x = 1
     C                   movea     alchrg        inf2(x)
    C*** BEGIN CHG *** 07/19/99 **************************************
    C**********           Z-ADD13        X
     C                   eval      x = 12
    C*** END   CHG *** 07/19/99 **************************************
     C                   movea     alpaid        inf2(x)
    C*** BEGIN CHG *** 07/19/99 **************************************
    C**********           ADD  14        X
     C                   eval      x = x + 12
    C*** END   CHG *** 07/19/99 **************************************
    C*** BEGIN ADD *** 07/19/99 **************************************
     C                   movea     scbtyp        inf2(x)
     C                   eval      x = x + 3
    C*** END   ADD *** 07/19/99 **************************************
    C*** BEGIN CHG *** 07/19/99 **************************************
    C**********           MOVELSCBTYP    INF2,X
     C                   movea     eoast         inf2(x)
    C*** BEGIN CHG *** 07/19/99 **************************************
     C                   movea     inf2          hlinf2
     C                   move      hlinf2        scdata
    C*** BEG ADD *** 09/27/00 ****************************************
    C*
    C* LOAD INFO #3
    C*
     C                   when      info# = 3
     C                   eval      x = 2
     C                   movea     eocntl        inf3(x)
     C                   eval      x = x + 20
     C                   movea     scblst        inf3(x)
     C                   movea     inf3          hlinf3
     C                   move      hlinf3        scdata
    C*
    C* LOAD INFO #4
    C*
     C                   when      info# = 4
     C                   eval      x = 2
     C                   movea     eopnam        inf4(x)
     C                   eval      x = x + 24
     C                   movea     ' '           inf4(x)
     C                   eval      x = x + 1
     C                   movea     scblst        inf4(x)
     C                   movea     inf4          hlinf4
     C                   move      hlinf4        scdata
    C*
    C*** BEG ADD *** 09/27/00 ****************************************
    C*
     C                   endsl
    C*
     C                   if        info# = 1
     C                   movel     heada         schdla
     C                   else
     C                   clear                   schdla
     C                   endif
    C*
    C* THE FOLLOWING ARE HIDDEN FIELDS THAT ARE NOT DISPLAYED BUT
    C* USED IN THIS PROGARM
    C*
     C                   movel(p)  eocert        sccert
     C                   eval      scext = eoext
    C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
    C*
     C                   eval      *in70 = *off
    C*
     C     *inkj         caseq     *on           kjdo
     C     *inkn         caseq     *on           kndo
     C                   endcs
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
    C*
    C*** BEGIN ADD *** 10/29/02 **************************************
     C                   if        *in55 = *on
    C*
     C                   move      @@call        svcall
     C                   move      *in09         svin09
    C*
     C                   if        scauth = 'I'
     C                   move      *on           *in09
     C                   move      '1'           @@call
     C                   move      '1'           ##call
     C                   endif
     C                   endif
    C*** END   ADD *** 10/29/02 **************************************
    C*** BEGIN ADD *** 01/11/99 **************************************
     C     scsel         caseq     '6'           opt6
    C*** END ADD *** 01/11/99 ****************************************
     C     scsel         caseq     '7'           opt7
     C     scsel         caseq     '8'           opt8
     C     scsel         caseq     '10'          opt10
     C     scsel         caseq     '11'          opt11
     C     scsel         caseq     '12'          opt12
     C     scsel         caseq     '13'          opt13
     C     scsel         caseq     '14'          opt14
     C     scsel         caseq     '15'          opt15
     C     scsel         caseq     '20'          opt20
    C*** BEG ADD *** 10/13/00 ****************************************
     C     scsel         caseq     '22'          opt22
    C*** END ADD *** 10/13/00 ****************************************
    C*** BEGIN ADD *** 04/08/99 **************************************
     C     scsel         caseq     '16'          opt16
    C*** BEGIN ADD *** 06/24/99 **************************************
     C     scsel         caseq     '30'          opt30
    C*** END   ADD *** 06/24/99 **************************************
     C     scsel         caseq     '81'          opt81
     C     scsel         caseq     '82'          opt82
    C*** END ADD *** 04/08/99 ****************************************
     C     scsel         caseq     '99'          opt99
     C*** BEG ADD *** 12/12/05 *********************************************
     C     scsel         caseq     '9'           opt9
     C*** END ADD *** 12/12/05 *********************************************
     C                   endcs
    C*** BEGIN ADD *** 10/29/02 **************************************
     C                   if        *in55 = *on
     C                   move      svcall        @@call
     C                   move      svin09        *in09
     C                   endif
    C*** END   ADD *** 10/29/02 **************************************
    C*
    C*** BEG ADD *** 11/19/99 ****************************************
    C*                    MOVEL'Y'       VLDOPT
    C*** END ADD *** 11/19/99 ****************************************
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
    C*** BEGIN ADD *** 10/29/02 **************************************
     C                   close     faeol69
     C                   close     faeol70
     C                   close     faeol71
     C                   close     faeol72
     C                   close     faeol73
    C*** END   ADD *** 10/29/02 **************************************
    C*** BEG ADD *** 02/23/00 ****************************************
     C                   if        qqflag = '0'
    C* Call CL to release the lock on Data area FDAILYA
     C                   call      'F0350C03'
     C                   endif
    C*** END ADD *** 02/23/00 ****************************************
    C*** BEGIN ADD *** 11/10/00 **************************************
     C                   if        stop = *on
     C                   move      *off          stop
    C*** BEGIN DEL *** 06/01/01 **************************************
    C**********           ELSE
    C*** END   ADD *** 11/10/00 **************************************
    C*** BEG ADD *** 05/18/00 ****************************************
    C********** ##WCIN    IFEQ 'Y'
    C*** BEG ADD *** 07/11/00 ****************************************
    C********** ##WCIF    ANDEQ'Y'
    C********** ##NPWC    ANDNE'Y'
    C*** END ADD *** 07/11/00 ****************************************
    C**********           CLOSEWDPALA
    C**********           ENDIF
    C*** END ADD *** 05/18/00 ****************************************
    C*** END   DEL *** 06/01/01 **************************************
    C*** BEGIN ADD *** 11/10/00 **************************************
     C                   endif
    C*** END   ADD *** 11/10/00 **************************************
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
    C*** BEG ADD *** 02/23/00 ****************************************
    C* Call CL to place a lock on Data area FDAILYA
     C                   call      'F0350C02'
     C                   parm      '0'           qqflag
    C*
     C                   if        qqflag = '1'
     C                   move      *on           *inlr
     C                   return
     C                   endif
    C*** END ADD *** 02/23/00 ****************************************
    C*
     C                   eval      *in12 = *on
    C*** BEGIN ADD *** 11/10/00 **************************************
     C                   move      *off          stop
    C*** END   ADD *** 11/10/00 **************************************
    C*** BEGIN ADD *** 10/29/02 **************************************
    C*
     C                   movel(p)  q1user        scuser
    C*
     C     scuser        chain(n)  pup01                              99
    C*
    C* Check for active user authorization
    C*
     C                   if        *in55 = *on
     C                   call      'SA008R'      rstpl                  99
     C                   move      upmrsu        ##mrsu
     C                   move      upmrgr        ##mrgr
     C                   move      upmrem        ##mrem
    C*
     C                   if        ##rcds = *on
     C                             and ##mrsu = 'Y'
     C                   exsr      getsub
     C                   endif
     C                   endif
    C*
     C                   open      faeol69
     C                   open      faeol70
     C                   open      faeol71
     C                   open      faeol72
     C                   open      faeol73
    C*** END   ADD *** 10/29/02 **************************************
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
    C*** BEG ADD *** 06/17/99 **************************************
     C                   parm                    ##mss
     C                   parm                    ##fsch
     C                   parm                    ##prvb
     C                   parm                    ##npwc
    C*** END ADD *** 06/17/99 **************************************
    C*
    C* Determine if OPT15=Pre-Cert Info should be displayed
    C*
     C                   if        ##ur = 'Y'
    C*** BEGIN CHG *** 07/29/99 **************************************
    C********             MOVELPCIOPT    OFD,14
     C                   movel     pciopt        ofd(15)
     C                   else
    C********             MOVELCLROPT    OFD,14
     C                   movel     clropt        ofd(15)
    C*** END CHG *** 07/29/99 ****************************************
     C                   endif
    C*** BEGIN ADD *** 04/08/99 **************************************
    C*
    C* Determine if OPT81=Claim Info should be displayed
    C*
     C                   if        ##wcif = 'Y'
     C                             and ##npwc <> 'Y'
    C*** BEGIN CHG *** 06/24/99 **************************************
    C*** BEGIN CHG *** 07/29/99 **************************************
    C*********            MOVELCLMOPT    OFD,21
    C********             MOVELCLMOPT    OFD,22
     C                   movel     clmopt        ofd(23)
    C*** END   CHG *** 06/24/99 **************************************
     C                   else
    C*** BEGIN CHG *** 06/24/99 **************************************
    C**********           MOVELCLROPT    OFD,21
    C********             MOVELCLROPT    OFD,22
     C                   movel     clropt        ofd(23)
    C*** END CHG *** 07/29/99 ****************************************
    C*** END   CHG *** 06/24/99 **************************************
     C                   endif
    C*** END ADD *** 04/08/99 ****************************************
    C*
    C*** BEG DEL *** 11/19/99 ****************************************
    C**********           MOVEL'N'       VLDOPT
    C*** END DEL *** 11/19/99 ****************************************
    C*** BEGIN DEL *** 06/01/01 **************************************
    C*** BEG ADD *** 05/18/00 ****************************************
    C********** ##WCIN    IFEQ 'Y'
    C*** BEG ADD *** 07/11/00 ****************************************
    C********** ##WCIF    ANDEQ'Y'
    C********** ##NPWC    ANDNE'Y'
    C*** END ADD *** 07/11/00 ****************************************
    C**********           OPEN WDPALA
    C**********           ENDIF
    C*** END ADD *** 05/18/00 ****************************************
    C*** END   DEL *** 06/01/01 **************************************
    C*
    C*** BEG ADD *** 06/02/99 ****************************************
    C*
    C** GET INITIAL VIEW FROM CONTROL FILE SFBPF (THEIR ARE 4 POSSIBLE VIEWS)
    C*
     C                   call      'SA009R'
 1   C                   parm      *zeros        ##eobd
 2   C                   parm      ' '           ##eobi
 3   C                   parm      ' '           ##arin
 4   C                   parm      ' '           ##acis
 5   C                   parm      ' '           ##pay0
 6   C                   parm      ' '           ##sdts
 7   C                   parm      ' '           ##mciv
 8   C                   parm      ' '           ##pbiv
     C                   if        ##pbiv = '1'
     C                             or ##pbiv = '2'
     C                             or ##pbiv = '3'
     C                             or ##pbiv = '4'
    C*** BEG ADD *** 03/29/00 ****************************************
     C                             or ##pbiv = '5'
    C*** BEG ADD *** 03/29/00 ****************************************
     C                   move      ##pbiv        view#
     C                   else
     C                   eval      view# = 1
     C                   endif
    C*** END ADD *** 06/02/99 ****************************************
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
    C*** BEGIN ADD *** 05/30/02 **************************************
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
    C*** END   ADD *** 05/30/02 **************************************
    C*** BEG ADD *** 11/19/99 ***************************************
     C                   move      'N'           vldopt
    C*** END ADD *** 11/19/99 ****************************************
    C*
     C                   select
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
     C                   if        *in21 = *on
     C     key31         reade     dbrcdu                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key31         reade     dbrcdv                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key31         reade     dbrcdw                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key31         reade     dbrcdg                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key31         reade     dbrc07                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
     C                   if        *in21 = *on
     C     key22         reade     dbrcdm                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key22         reade     dbrcdn                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key22         reade     dbrcdo                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key22         reade     dbrcdp                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key22         reade     dbrc08                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY11     READEDBRCDH                   95
    C***22***** KEY11     READEDBRCDD                   95
    C***23***** KEY11     READEDBRCDI                   95
    C***24***** KEY11     READEDBRCDJ                   95
     C                   if        *in21 = *on
     C     key11         reade     dbrcdh                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key11         reade     dbrcdi                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key11         reade     dbrc06                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key11         reade     dbrcdi                                 95
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key11         reade     dbrc09                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
     C                   if        *in21 = *on
     C     key9          reade     dbrcdc                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key9          reade     dbrcdd                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key9          reade     dbrcde                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key9          reade     dbrcdg                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key9          reade     dbrc10                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY15     READEDBRCDK                   95
    C***22***** KEY15     READEDBRCD2                   95
    C***23***** KEY15     READEDBRCDL                   95
    C***24***** KEY15     READEDBRCD2                   95
     C                   if        *in21 = *on
     C     key15         reade     dbrcdk                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key15         reade     dbrc05                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key15         reade     dbrcdl                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key15         reade     dbrc05                                 95
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key15         reade     dbrc11                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in21 = *on
     C     key35         reade     dbrcdx                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key35         reade     dbrcdy                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key35         reade     dbrcdz                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key35         reade     dbrcd4                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key35         reade     dbrc12                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
    C*
     C                   when      sssub# <> *zeros and ssfnd <> *zeros
     C                   if        *in21 = *on
     C     key67         reade     dbrc25                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key67         reade     dbrc26                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key67         reade     dbrc27                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key67         reade     dbrc28                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key67         reade     dbrc29                                 95
     C                   endif
    C*
     C                   when      sssub# <> *zeros
     C                   if        *in21 = *on
     C     key61         reade     dbrc20                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key61         reade     dbrc21                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key61         reade     dbrc22                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key61         reade     dbrc23                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key61         reade     dbrc24                                 95
     C                   endif
    C*** END   ADD *** 05/30/02 **************************************
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C                   when      ssbtyp <> *blanks
     C                   if        *in21 = *on
     C     key55         reade     dbrc15                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key55         reade     dbrc16                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key55         reade     dbrc17                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key55         reade     dbrc18                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key55         reade     dbrc19                                 95
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
     C                   when      ssiusr <> *blanks
     C                   if        *in21 = *on
     C     key27         reade     dbrcdq                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key27         reade     dbrcdr                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key27         reade     dbrcds                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key27         reade     dbrcdt                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key27         reade     dbrc14                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   other
     C                   if        *in21 = *on
     C     key           reade     dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key           reade     dbrcd2                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key           reade     dbrcd3                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key           reade     dbrcd4                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key           reade     dbrcd5                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
    C*** BEGIN ADD *** 05/30/02 **************************************
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
    C*** END   ADD *** 05/30/02 **************************************
    C*** BEG ADD *** 11/19/99 ****************************************
     C                   move      'N'           vldopt
    C*** END ADD *** 11/19/99 ****************************************
    C*
     C                   select
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
     C                   if        *in21 = *on
     C     key31         readpe    dbrcdu                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key31         readpe    dbrcdv                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key31         readpe    dbrcdw                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key31         readpe    dbrcdg                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key31         readpe    dbrc07                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
     C                   if        *in21 = *on
     C     key22         readpe    dbrcdm                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key22         readpe    dbrcdn                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key22         readpe    dbrcdo                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key22         readpe    dbrcdp                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key22         readpe    dbrc08                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY11     REDPEDBRCDH                   95
    C***22***** KEY11     REDPEDBRCDD                   95
    C***23***** KEY11     REDPEDBRCDI                   95
    C***24***** KEY11     REDPEDBRCDJ                   95
     C                   if        *in21 = *on
     C     key11         readpe    dbrcdh                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key11         readpe    dbrcdi                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key11         readpe    dbrc06                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key11         readpe    dbrcdi                                 95
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key11         readpe    dbrc09                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   if        *in21 = *on
     C     key9          readpe    dbrcdc                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key9          readpe    dbrcdd                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key9          readpe    dbrcde                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key9          readpe    dbrcdg                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key9          readpe    dbrc10                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY15     REDPEDBRCDK                   95
    C***22***** KEY15     REDPEDBRCD2                   95
    C***23***** KEY15     REDPEDBRCDL                   95
    C***24***** KEY15     REDPEDBRCD2                   95
     C                   if        *in21 = *on
     C     key15         readpe    dbrcdk                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key15         readpe    dbrc05                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key15         readpe    dbrcdl                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key15         readpe    dbrc05                                 95
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key15         readpe    dbrc11                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in21 = *on
     C     key35         readpe    dbrcdx                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key35         readpe    dbrcdy                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key35         readpe    dbrcdz                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key35         readpe    dbrcd4                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key35         readpe    dbrc12                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
    C*
     C                   when      sssub# <> *zeros and ssfnd <> *zeros
     C                   if        *in21 = *on
     C     key67         readpe    dbrc25                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key67         readpe    dbrc26                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key67         readpe    dbrc27                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key67         readpe    dbrc28                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key67         readpe    dbrc29                                 95
     C                   endif
    C*
     C                   when      sssub# <> *zeros
     C                   if        *in21 = *on
     C     key61         readpe    dbrc20                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key61         readpe    dbrc21                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key61         readpe    dbrc22                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key61         readpe    dbrc23                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key61         readpe    dbrc24                                 95
     C                   endif
    C*** END   ADD *** 05/30/02 **************************************
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C                   when      ssbtyp <> *blanks
     C                   if        *in21 = *on
     C     key55         readpe    dbrc15                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key55         readpe    dbrc16                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key55         readpe    dbrc17                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key55         readpe    dbrc18                                 95
     C                   endif
     C                   if        *in25 = *on
     C     key55         readpe    dbrc19                                 95
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
     C                   when      ssiusr <> *blanks
     C                   if        *in21 = *on
     C     key27         readpe    dbrcdq                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key27         readpe    dbrcdr                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key27         readpe    dbrcds                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key27         readpe    dbrcdt                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key27         readpe    dbrc14                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   other
     C                   if        *in21 = *on
     C     key           readpe    dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key           readpe    dbrcd2                                 95
     C                   endif
     C                   if        *in23 = *on
     C     key           readpe    dbrcd3                                 95
     C                   endif
     C                   if        *in24 = *on
     C     key           readpe    dbrcd4                                 95
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key           readpe    dbrcd5                                 95
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
    C*
     C                   select
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
    C***21      KEY14     SETGTDBRCDU               96
    C***22      KEY15     SETGTDBRCDV               96
    C***23      KEY16     SETGTDBRCDW               96
    C***24      KEY31     SETGTDBRCDQ               96
    C*
     C                   if        *in21 = *on
     C     key28         setgt     dbrcdu                             96
     C                   endif
     C                   if        *in22 = *on
     C     key29         setgt     dbrcdv                             96
     C                   endif
     C                   if        *in23 = *on
     C     key30         setgt     dbrcdw                             96
     C                   endif
     C                   if        *in24 = *on
     C     key31         setgt     dbrcdg                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key41         setgt     dbrc07                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
     C                   if        *in21 = *on
     C     key18         setgt     dbrcdm                             96
     C                   endif
     C                   if        *in22 = *on
     C     key19         setgt     dbrcdn                             96
     C                   endif
     C                   if        *in23 = *on
     C     key20         setgt     dbrcdo                             96
     C                   endif
     C                   if        *in24 = *on
     C     key21         setgt     dbrcdp                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key42         setgt     dbrc08                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY10     SETGTDBRCDH               96
    C***22***** KEY11     SETGTDBRCDD               96
    C***23***** KEY12     SETGTDBRCDI               96
    C***24***** KEY13     SETGTDBRCDJ               96
     C                   if        *in21 = *on
     C     key10         setgt     dbrcdh                             96
     C                   endif
     C                   if        *in22 = *on
     C     key11a        setgt     dbrcdi                             96
     C                   endif
     C                   if        *in23 = *on
     C     key12         setgt     dbrc06                             96
     C                   endif
     C                   if        *in24 = *on
     C     key13         setgt     dbrcdi                             96
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key43         setgt     dbrc09                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                   if        *in21 = *on
     C     key5          setgt     dbrcdc                             96
     C                   endif
     C                   if        *in22 = *on
     C     key6          setgt     dbrcdd                             96
     C                   endif
     C                   if        *in23 = *on
     C     key7          setgt     dbrcde                             96
     C                   endif
     C                   if        *in24 = *on
     C     key8          setgt     dbrcdg                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key44         setgt     dbrc10                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY14     SETGTDBRCDK               96
    C***22***** KEY15     SETGTDBRCD2               96
    C***23***** KEY16     SETGTDBRCDL               96
    C***24***** KEY17     SETGTDBRCD2               96
     C                   if        *in21 = *on
     C     key14         setgt     dbrcdk                             96
     C                   endif
     C                   if        *in22 = *on
     C     key15a        setgt     dbrc05                             96
     C                   endif
     C                   if        *in23 = *on
     C     key16         setgt     dbrcdl                             96
     C                   endif
     C                   if        *in24 = *on
     C     key17         setgt     dbrc05                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key45         setgt     dbrc11                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** END   CHG *** 04/27/99 **************************************
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in21 = *on
     C     key32         setgt     dbrcdx                             96
     C                   endif
     C                   if        *in22 = *on
     C     key33         setgt     dbrcdy                             96
     C                   endif
     C                   if        *in23 = *on
     C     key34         setgt     dbrcdz                             96
     C                   endif
     C                   if        *in24 = *on
     C     key35         setgt     dbrcd4                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key46         setgt     dbrc12                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
    C*
     C                   when      sssub# <> *zeros and ssfnd <> *zeros
     C                   if        *in21 = *on
     C     key62         setgt     dbrc25                             96
     C                   endif
     C                   if        *in22 = *on
     C     key63         setgt     dbrc26                             96
     C                   endif
     C                   if        *in23 = *on
     C     key64         setgt     dbrc27                             96
     C                   endif
     C                   if        *in24 = *on
     C     key65         setgt     dbrc28                             96
     C                   endif
     C                   if        *in25 = *on
     C     key66         setgt     dbrc29                             96
     C                   endif
    C*
     C                   when      sssub# <> *zeros
     C                   if        *in21 = *on
     C     key56         setgt     dbrc20                             96
     C                   endif
     C                   if        *in22 = *on
     C     key57         setgt     dbrc21                             96
     C                   endif
     C                   if        *in23 = *on
     C     key58         setgt     dbrc22                             96
     C                   endif
     C                   if        *in24 = *on
     C     key59         setgt     dbrc23                             96
     C                   endif
     C                   if        *in25 = *on
     C     key60         setgt     dbrc24                             96
     C                   endif
    C*** END   ADD *** 05/30/02 **************************************
    C*
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C                   when      ssbtyp <> *blanks
     C                   if        *in21 = *on
     C     key50         setgt     dbrc15                             96
     C                   endif
     C                   if        *in22 = *on
     C     key51         setgt     dbrc16                             96
     C                   endif
     C                   if        *in23 = *on
     C     key52         setgt     dbrc17                             96
     C                   endif
     C                   if        *in24 = *on
     C     key53         setgt     dbrc18                             96
     C                   endif
     C                   if        *in25 = *on
     C     key54         setgt     dbrc19                             96
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
     C                   when      ssiusr <> *blanks
     C                   if        *in21 = *on
     C     key23         setgt     dbrcdq                             96
     C                   endif
     C                   if        *in22 = *on
     C     key24         setgt     dbrcdr                             96
     C                   endif
     C                   if        *in23 = *on
     C     key25         setgt     dbrcds                             96
     C                   endif
     C                   if        *in24 = *on
     C     key26         setgt     dbrcdt                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key48         setgt     dbrc14                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   other
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setgt     dbrcd2                             96
     C                   endif
     C                   if        *in23 = *on
     C     key3          setgt     dbrcd3                             96
     C                   endif
     C                   if        *in24 = *on
     C     key4          setgt     dbrcd4                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key49         setgt     dbrcd5                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
    C*
     C                   select
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sseob# <> *zeros
     C                   if        *in21 = *on
     C     key28         setll     dbrcdu                             96
     C                   endif
     C                   if        *in22 = *on
     C     key29         setll     dbrcdv                             96
     C                   endif
     C                   if        *in23 = *on
     C     key30         setll     dbrcdw                             96
     C                   endif
     C                   if        *in24 = *on
     C     key31         setll     dbrcdg                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key41         setll     dbrc07                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and sscert <> *blanks
     C                   if        *in21 = *on
     C     key18         setll     dbrcdm                             96
     C                   endif
     C                   if        *in22 = *on
     C     key19         setll     dbrcdn                             96
     C                   endif
     C                   if        *in23 = *on
     C     key20         setll     dbrcdo                             96
     C                   endif
     C                   if        *in24 = *on
     C     key21         setll     dbrcdp                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key42         setll     dbrc08                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
     C                             and ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY10     SETLLDBRCDH               96
    C***22***** KEY11     SETLLDBRCDD               96
    C***23***** KEY12     SETLLDBRCDI               96
    C***24***** KEY13     SETLLDBRCDJ               96
     C                   if        *in21 = *on
     C     key10         setll     dbrcdh                             96
     C                   endif
     C                   if        *in22 = *on
     C     key11a        setll     dbrcdi                             96
     C                   endif
     C                   if        *in23 = *on
     C     key12         setll     dbrc06                             96
     C                   endif
     C                   if        *in24 = *on
     C     key13         setll     dbrcdi                             96
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key43         setll     dbrc09                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sssub# <> *zeros
     C                             and ssfnd <> *zeros
     C                             and ssfyr <> *zeros
     C                             and sscase <> *zeros
    C*
     C                   if        *in21 = *on
     C     key5          setll     dbrcdc                             96
     C                   endif
     C                   if        *in22 = *on
     C     key6          setll     dbrcdd                             96
     C                   endif
     C                   if        *in23 = *on
     C     key7          setll     dbrcde                             96
     C                   endif
     C                   if        *in24 = *on
     C     key8          setll     dbrcdg                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key44         setll     dbrc10                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      ssfein <> *zeros
    C*** BEGIN CHG *** 04/27/99 **************************************
    C***21***** KEY14     SETLLDBRCDK               96
    C***22***** KEY15     SETLLDBRCD2               96
    C***23***** KEY16     SETLLDBRCDL               96
    C***24***** KEY17     SETLLDBRCD2               96
     C                   if        *in21 = *on
     C     key14         setll     dbrcdk                             96
     C                   endif
     C                   if        *in22 = *on
     C     key15a        setll     dbrc05                             96
     C                   endif
     C                   if        *in23 = *on
     C     key16         setll     dbrcdl                             96
     C                   endif
     C                   if        *in24 = *on
     C     key17         setll     dbrc05                             96
     C                   endif
    C*** END   CHG *** 04/27/99 **************************************
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key45         setll     dbrc11                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   when      sseob# <> *zeros
     C                   if        *in21 = *on
     C     key32         setll     dbrcdx                             96
     C                   endif
     C                   if        *in22 = *on
     C     key33         setll     dbrcdy                             96
     C                   endif
     C                   if        *in23 = *on
     C     key34         setll     dbrcdz                             96
     C                   endif
     C                   if        *in24 = *on
     C     key35         setll     dbrcd4                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key46         setll     dbrc12                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*** BEGIN ADD *** 05/30/02 **************************************
    C*
     C                   when      sssub# <> *zeros and ssfnd <> *zeros
     C                   if        *in21 = *on
     C     key62         setll     dbrc25                             96
     C                   endif
     C                   if        *in22 = *on
     C     key63         setll     dbrc26                             96
     C                   endif
     C                   if        *in23 = *on
     C     key64         setll     dbrc27                             96
     C                   endif
     C                   if        *in24 = *on
     C     key65         setll     dbrc28                             96
     C                   endif
     C                   if        *in25 = *on
     C     key66         setll     dbrc29                             96
     C                   endif
    C*
     C                   when      sssub# <> *zeros
     C                   if        *in21 = *on
     C     key56         setll     dbrc20                             96
     C                   endif
     C                   if        *in22 = *on
     C     key57         setll     dbrc21                             96
     C                   endif
     C                   if        *in23 = *on
     C     key58         setll     dbrc22                             96
     C                   endif
     C                   if        *in24 = *on
     C     key59         setll     dbrc23                             96
     C                   endif
     C                   if        *in25 = *on
     C     key60         setll     dbrc24                             96
     C                   endif
    C*** END   ADD *** 05/30/02 **************************************
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C                   when      ssbtyp <> *blanks
     C                   if        *in21 = *on
     C     key50         setll     dbrc15                             96
     C                   endif
     C                   if        *in22 = *on
     C     key51         setll     dbrc16                             96
     C                   endif
     C                   if        *in23 = *on
     C     key52         setll     dbrc17                             96
     C                   endif
     C                   if        *in24 = *on
     C     key53         setll     dbrc18                             96
     C                   endif
     C                   if        *in25 = *on
     C     key54         setll     dbrc19                             96
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
     C                   when      ssiusr <> *blanks
     C                   if        *in21 = *on
     C     key23         setll     dbrcdq                             96
     C                   endif
     C                   if        *in22 = *on
     C     key24         setll     dbrcdr                             96
     C                   endif
     C                   if        *in23 = *on
     C     key25         setll     dbrcds                             96
     C                   endif
     C                   if        *in24 = *on
     C     key26         setll     dbrcdt                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key48         setll     dbrc14                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
    C*
     C                   other
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setll     dbrcd2                             96
     C                   endif
     C                   if        *in23 = *on
     C     key3          setll     dbrcd3                             96
     C                   endif
     C                   if        *in24 = *on
     C     key4          setll     dbrcd4                             96
     C                   endif
    C*** BEG ADD *** 03/08/00 ****************************************
     C                   if        *in25 = *on
     C     key49         setll     dbrcd5                             96
     C                   endif
    C*** END ADD *** 03/08/00 ****************************************
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
    C*
    C*** BEGIN ADD *** 04/03/00 **************************************
     C                   eval      @@eob# = sceob#
    C*
     C                   if        q1prms > 11
    C*
     C                   eval      @@rcn = scrcn
    C*
     C                   endif
    C*** END   ADD *** 04/03/00 **************************************
    C*
     C                   endsr
    C*================================================================
    C* SSCLER - Clear subset description fields
    C*================================================================
     C     sscler        begsr
    C*
    C* CLEAR ALL SUBSET DESCRIPTIONS IF CORRESPONDING SUBSET FIELD
    C* IS EMPTY
    C*
     C                   if        sssub# = *zeros
     C                   clear                   sssmnm
     C                   endif
    C*
     C                   if        ssfnd = *zeros
     C                   clear                   ssfnnm
     C                   endif
    C*
     C                   if        ssfyr = *zeros
     C                             and sscase = *zeros
     C                   clear                   ssname
     C                   endif
    C*
     C                   if        ssfein = *zeros
     C                             and ssmod = *blanks
     C                   clear                   sspdnm
     C                   endif
    C*
     C                   if        ssiusr = *blanks
     C                   clear                   ssusnm
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C*** BEG ADD ****** 05/25/07 *************************************************
     C*** convert MC GRP# to WC FND#
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
     C*** Check claim authority
     C                   move      #1wcf#        #1fnd
     C                   move      eofyr         #1fyr
     C                   move      eocase        #1case
     C                   move      *blanks       #1rtcd
     C*
     C                   call      'WA067R'      cuspl
     C* not authorised when rtcd = '2'
     C                   if        #1rtcd = '2'
     C                   eval      meetss = *off
     C                   endif
     C*** END ADD ****** 05/25/07 *************************************************
     C*
     C                   if        sssub# <> *zeros
     C                             and sssub# <> eosub#
     C                             or sssub# <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssfnd <> *zeros
     C                             and ssfnd <> eofnd
     C                             or ssfnd  <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssfyr <> *zeros
     C                             and ssfyr <> eofyr
     C                             or ssfyr  <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        sscase <> *zeros
     C                             and sscase <> eocase
     C                             or sscase <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssfein <> *zeros
     C                             and ssfein <> eofein
     C                             or ssfein <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
    C*** BEGIN DEL *** 04/27/99 **************************************
    C**********           MOVELSSMOD     SXMOD
    C*** END   DEL *** 04/27/99 **************************************
     C                   movel     ssmod         xxmod
     C                   if        ssmod <> *blanks
     C                             and xxmod <> eomod
     C                             or ssmod  <> *zeros
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*** BEGIN ADD *** 12/27/98 **************************************
    C*
     C                   if        sseob# <> *zeros
     C                             and sseob# <> eoeob#
     C                             or sseob# <> *zeros
     C                             and eoblst <> 'P'
    C*
     C                   movel(p)  *off          meetss
    C*
     C                   endif
    C*
     C                   if        sscert <> *blanks
     C                             and sscert <> eocert
     C                             or sscert <> *blanks
     C                             and eoblst <> 'P'
    C*
     C                   movel(p)  *off          meetss
    C*
     C                   endif
    C*
    C*** END   ADD *** 12/27/98 **************************************
    C*
    C* SUBSET DATE OF SERVICE SHOULD FALL WITHIN EOMB SERVICE DATE
    C* RANGE (EOFRDT - EOTODT)
    C*
     C                   eval      srdti = ssfrdt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hlfrdt
     C                   eval      srdti = sstodt
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hltodt
    C********** HLFRDT    IFNE *ZEROS
    C********** HLFRDT    IFLT EOFRDT
    C********** HLFRDT    ORGT EOTODT
    C**********           MOVE *OFF      MEETSS
    C**********           ENDIF                           HLFRDT LT EOFRDT
    C**********           ENDIF                           HLFRDT NE *ZEROS
    C*
    C* SUBSET CRITERIA WHEN ONLY FROM DATE IS ENTERED
    C*
     C                   if        hlfrdt > *zeros
     C                             and hltodt = *zeros
     C                   if        eofrdt < hlfrdt
     C                             and eotodt < hlfrdt
     C                             or  eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
    C* SUBSET CRITERIA WHEN RANGE IS SPECIFIED
    C*
     C                   if        hlfrdt > *zeros
     C                             and hltodt > *zeros
    C*
     C                   if        eotodt < hlfrdt
     C                             or eofrdt > hltodt
     C                             or  eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*
     C                   endif
    C*** BEGIN ADD *** 06/01/01 **************************************
    C*
     C                   if        ssbtyp <> *blanks
     C                             and ssbtyp <> eobtyp
     C                             or ssbtyp <> *blanks
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*** END   ADD *** 06/01/01 **************************************
    C*
     C                   if        ssiusr <> *blanks
     C                             and ssiusr <> eoiusr
     C                             or ssiusr <> *blanks
     C                             and eoblst <> 'P'
     C                   move      *off          meetss
     C                   endif
    C*** BEGIN ADD *** 10/29/02 **************************************
     C                   if        *in55 = *on
    C*
     C                   move      'A'           scauth
    C*
    C* Check Subscriber Restriction
    C*
     C                   if        ##mrsu = 'Y'
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
     C                   if        ##rtcd = *on
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
    C*  Check Group restriction
    C*
     C                   if        ##mrgr = 'Y'
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
     C                   if        ##rtcd = *on
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
    C*  Check for Employer Restriction
    C*
     C                   if        ##mrgr = 'Y'
     C                             and ##rcds = *on
     C                             and meetss = *on
     C*
     C     cmky1         chain     pcm01                              99
     C                   if        *in99 = *off
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
     C                   if        ##rtcd = *on
     C                   move      *off          meetss
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*** END   ADD *** 10/29/02 **************************************
    C*** BEG   ADD *** 08/08/05 **************************************
     C                   if        ##clt# = '135' and eombup <> 'C'
     C                   move      *off          meetss
     C                   endif
    C*** END   ADD *** 08/08/05 **************************************
     C                   endsr
** Column headings
Opt Claimant                EOMB#    Provider       From          To      Sts
Opt Provider       Claimant                EOMB#    From          To      Sts
Opt Soc Sec #     DOI        EOMB#   Provider       From          To      Sts
Opt    EOMB#   Claimant             Provider        From          To      Sts
Opt Control Number Claimant                EOMB#    From          To      Sts
Opt Claimant                EOMB#    Provider    Charge Amt   Paid Amt Typ AST
Opt Provider       Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Soc Sec #     DOI        EOMB#   Provider    Charge Amt   Paid Amt Typ AST
Opt    EOMB#   Claimant             Provider     Charge Amt   Paid Amt Typ AST
Opt Control Number Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Claimant                EOMB#    Provider     Control Number     Sts
Opt Provider       Claimant                EOMB#  Control Number     Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Control Number     Sts
Opt    EOMB#   Claimant             Provider      Control Number     Sts
Opt Control Number Claimant                EOMB#  Control Number     Sts
Opt Claimant                EOMB#    Provider     Provider's Name         Sts
Opt Provider       Claimant                EOMB#  Provider's Name         Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Provider's Name         Sts
Opt    EOMB#   Claimant             Provider      Provider's Name         Sts
Opt Control Number Claimant                EOMB#  Provider's Name         Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN     07/29/99
1044=Delete                 YNN
1055=Display                YYY
1066=Dtl Chrg List          YNN
1077=Work w/ Detail         YYY
1088=Work w/ Amounts        YYN
1099=Complete               YNN
1099=EOMB Delete            NNN
11010=EOMB status           YYY
11111=EOB/Notes             YYY
11212=Print EOMB            YYY
11313=Display EOMB          YYY
11414=DARTS                 YNN
11515=Pre-Cert Info         YYY
11616=History               NNN
11818=EDI Log Info          NNN
11919=Send Subsq 837        NNN
12020=Pended items          YYY
12121=Check info            NNN
12222=View Prov Info        YYY
13030=Letters               YYN
18181=Claim info            YYN
18282=Case info             YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYN
211F11=Change view          YYN
212F12=Cancel               YYY
215F15=Fold/Truncate        YYN
217F17=Subset               YYY
3
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  )
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FTABL)
DLTOVR FILE(FMPRL)
DLTOVR FILE(FTPSL)
DLTOVR FILE(FMSVP)
DLTOVR FILE(FMSVL1)
DLTOVR FILE(FMSVL2)
DLTOVR FILE(FMPRP)
