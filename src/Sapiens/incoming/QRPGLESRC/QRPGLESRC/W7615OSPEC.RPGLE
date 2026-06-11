     O*****************************************************************
     O* REVISIONS:                                                    *
     O*                                                               *
     O* 11/22/13 C:279 Project: 129WSB (Implement 129WJI into Base)   *
     O*                Do not print "Make checks payable to" on bills.*
     O*                                                               *
     O* 10/13/11 C:191 Project: WC2079                                *
     O*                Add -ve sign after XXEMPR if -ve amount        *
     O*                                                               *
     O* 06/06/11 C:58  Project: WC1995                                *
     O*                Change estimated, revised & audited bills      *
     O*                caption "Deductible Limit" to "Aggregate"      *
     O*                                                               *
     O* 01/24/11 C:136 Support: WA089 75611                           *
     O*                Correct printing of payrolls on summary bill   *
     O*                by removing the extension field. There isn't   *
     O*                enough room.                                   *
     O*                                                               *
     O* 11/19/09  C:33   Project: 25WQI                               *
     O*                  Print Statement #.                           *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use Payroll Extension Description              *
     H*                                                               *
     H*                                                               *
     H* 07/17/08 C:136 SUPPORT: WA099 63212                           *
     H*                PREVENT 'WEEKLY INSTALLMENTS' FROM PRINTING    *
     H*                AT THE SAME TIME AS 'INSTALLMENTS ON 1ST/15TH' *
     H*                                                               *
     H* 06/05/08 C:65   PROJECT: 46WFM                                *
     H*                 ADD LINE ITEM FOR FEES BILLED TO LSAT OUTPUT. *
     H*                                                               *
     H* 05/04/07 C:136 SUPPORT: WA045 58218                           *
     H*                DEBIT MESSAGE MISSING FROM TOTA                *
     H*                                                               *
     H* 04/06/07 C:65  PROJECT: WC1514                                *
     H*                MOVE EFF/EXP DATE COLUMN HEADINGS TO PRINT ON  *
     H*                2ND LINE ALL THE TIME.                         *
     H*                                                               *
     H* 01/11/05 C:180 PROJECT: 114WMF                                *
     H*                - ADD 6TH SLOT TO EMPLR NAME/ADDRESS ARRAY FOR *
     H*                  BILLING CONTACT NAME, ON THE 1ST PAGE ONLY.  *
     H*                                                               *
     H* 09/13/06 C:180 PROJECT: 114WMF                                *
     H*                 ADD A NEW PAGE TO THE BILL FOR THE RETRO      *
     H*                 INFO RATING SHEET.                            *
     H*                                                               *
     H* 06/27/06 C:65  PROJECT: 109WDQ                                *
     H*                ADD LINE THAT CAN BE USED IN A GENERIC WAY     *
     H*                TO OUTPUT(133WCI) TO SUMMARY BILL AS WELL.     *
     H*                                                               *
     H* 06/12/06 C:136 SUPPORT: WA132 53105                           *
     H*                CORRECT ARRANGEMENT OF INSTALLMENTS ON         *
     H*                SUMMARY EXCEPTION (SUME)                       *
     H*                                                               *
     H* 04/17/06 C:168  PROJECT: 46WFM (COPY OF 46WFD)                *
     H*                 Change the total section to show 'Billed'     *
     H*                 'Paid' 'Balance Due' for each line item.      *
     H*                 Add NSF/Legal Fees.                           *
     H*                                                               *
     H* 08/01/05 C:136 PROJECT: 133WCI (ADD FEES BILLED)              *
     H*                ADD LINE THAT CAN BE USED IN A GENERIC WAY     *
     H*                TO OUTPUT                                      *
     H*                                                               *
     H* 04/07/05  C:156PROJECT: 126WDK                                *
     H*                PROJECT: 126WCD                                *
     H*                Multiple Deductible discounts per Year.        *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288 (SUMMARIZE S/R POLICIES)       *
     H*                AFFECT CHANGES IN BILL TO SUMMARIZE S/R        *
     H*                POLICIES                                       *
     H*                                                               *
     H* 11/29/04  C:65 PROJECT: SUPP# 43065                           *
     H*                ENSURE % SIGNS DO NOT PRINT WHEN BALANCE = 'N' *
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER VARIABLE FIELD                *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                    *
     H*                Add 4th and 5th assessments.                   *
     H*                                                               *
     H* 04/26/04 C:136 PROJECT: WC1169                                *
     H*                DISPLAY 'CHARGE' FOR OCCUPATIONAL CLASS        *
     H*                                                               *
     H* 10/21/03 C:65  PROJECT: WC1017                                *
     H*                PRINT RENEWAL QUOTE IF PROCESSING A QUOTED     *
     H*                RENEWAL.                                       *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                ADD DATE RANGE FOR FL 4/1/3 RATE CHANGE.       *
     H*                                                               *
     H* 04/25/03 C:65  PROJECT: WC1115                                *
     H*                ADD DIFFERENT WORDING FOR TWICE A MONTH BSCD.  *
     H*                                                               *
     H* 04/21/03 C:65  PROJECT: WC1115                                *
     H*                PRINT SPECIAL WORDING FOR WEEKLY BILLING CODES *
     H*                                                               *
     H* 06/14/02  C:136 SUPPORT: WA0122 SEQ: 33002                    *
     H*                 Last line of summary page SUME and SUMET      *
     H*                 prints on next page. Adjusted printing of     *
     H*                 due date array to compensate for the addition *
     H*                 of the installment fee line item.             *
     H*                                                               *
     H* 02/07/02  C:65  PROJECT: WC1067                               *
     H*                 CHANGE INDICATOR ON MINIMUM PREMIUM SO THE    *
     H*                 EXPENSE CONSTANT CAN PRINT WHEN BILLED ON     *
     H*                 MINIMUM.                                      *
     H*                                                               *
     H* 11/21/01  C:90  PROJECT: SEQ#29878                            *
     H*                 Modify output to fit expanded premium field.  *
     H*                                                               *
     H* 08/30/01  C:90  PROJECT: WC1052                               *
     H*                 Modify output to fit expanded premium field.  *
     H*                                                               *
     H* 07/02/01  C:90  PROJECT: WC1028                               *
     H*                 Print "File #" instead of "Policy #" if a     *
     H*                 carrier policy number has been entered.       *
     H*                                                               *
     H* 02/21/01  C:65  PROJECT: WC1026                               *
     H*                 PRINT MERIT RATING INSTEAD OF EXP MOD WHEN    *
     H*                 APPLICABLE.                                   *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT: WC908                                *
     H*                 PULL STANDARD INCR LIMITS FROM STATISTICAL    *
     H*                                                               *
     H* 09/09/99  C:65  PROJECT: SUPPORT#19580                        *
     H*                 MOVE INSTALLMENT OVER 1 SPACE TO PREVENT      *
     H*                 CUTTING OFF OF YEAR FOR SEPTEMBER INSTM.      *
     H*                 THE CHANGE IS DONE TO OUTPUT SPECS TOTE,      *
     H*                 TOTE1, TOTET, AND TOTET1 MOVING AMT ARRAY     *
     H*                 FROM 43 TO 44.                                *
     H*                                                               *
     H* 08/24/99  C:24  PROJECT: WC902                                *
     H*                 PRINT 'PER OCCURRENCE' OR 'PER CLAIM' BASED   *
     H*                 ON THE DDVERB FLAG.                           *
     H*                                                               *
     H* 04/12/99  C:65  SUPPORT: WC875                                *
     H*                 PRINT STATE TAX ON DEPOSIT FOR S/R EMPLOYERS  *
     H*                 WITHIN THE DEPOSIT SECTION OF BALANCE INFO.   *
     H*                                                               *
     H* 03/04/99  C:87  PROJECT: WC885                                *
     H*                 Condition credit and debit messages for audit *
     H*                 bill with indicators based on WMEXP           *
     H*                                                               *
     H* 01/25/99  C:65  SUPPORT: WC881                                *
     H*                 PRINT 'DUE THRU' ON FIRST REVISED INSTALLMENT *
     H*                                                               *
     H* 11/12/98  C:65  PROJECT: SUPPORT                              *
     H*                 DISPLAY STANDARD LIABILITY LIMITS ON 1ST PAGE.*
     H*                                                               *
     H* 09/11/98  C:65  PROJECT: WC705                                *
     H*                 MOVE THE WINDOW BOX ADDRESS UP TWO LINES.     *
     H*                                                               *
     H* 09/08/98  C:65  PROJECT: WC705                                *
     H*                 ADD CARRIER POLICY NUMBER TO THE BILL.        *
     H*                                                               *
     H* 07/17/98  C:65  PROJECT: WC847                                *
     H*                 MOVE THE AMOUNT COLUMN FOR THE BALANCE INFO   *
     H*                 TO THE RIGHT SO WE CAN FIT IN THE ASSESSMENT  *
     H*                 PERCENTAGES TO THE RIGHT OF THEIR HEADING.    *
     O*                                                               *
     O* 06/01/98  C:65  PROJECT CM039                                 *
     O*                 CHANGE NAME FOR MAKE CHECKS PAYABLE TO.       *
     O*                                                               *
     O* 05/29/98  C:65  PROJECT WC705                                 *
     O*                 USE FLEXIBLE HEADING FOR STATE TAX.           *
     O*                                                               *
     O* 05/22/98  C:65  PROJECT WC705                                 *
     O*                 ADD THE PRINT BALANCE INDICATOR TO EST BILLS. *
     O*                                                               *
     O* 03/24/98  C:65  PROJECT WC705                                 *
     O*                 CHANGE POLICY # TO QUOTE # FOR QUOTE BILLS.   *
     O*                 CHANGE DATE OF BILL TO PRINT DATE.            *
     O*                                                               *
     O* 03/09/98  C:65  PROJECT WC705                                 *
     O*                 REMOVE EXTRA INDICATOR FOR SELF-RPT TOTAL     *
     O*                                                               *
     O* 03/04/98  C:65  PROJECT WC705                                 *
     O*                 ADD DUE DATE TO AUDIT BILLS.                  *
     O*                                                               *
     O* 02/04/98  C:65  PROJECT WC705                                 *
     O*                 REMOVE PAYMENT PLAN FOR AUDIT BILLS.          *
     O*
     O* 01/20/98  C:65  PROJECT WC705
     O*                 ADD RATING STATE AND USER TO FIRST PAGE OF BILL
     O*
     O* 12/23/97  C:65  PROJECT WC776
     O*                 ADD NEW OSPECS FOR ASSESSMENT FEES.
     O*
     O* 11/20/97  C:65  PROJECT V3
     O*                 ADDED INDICATORS FOR PRINTING OF SAFETY AND
     O*                 MEMBERSHIP FEES DUE WHEN ZERO.
     O*****************************************************************
     O*
     O* QSYSPRT - RESOLVED DARTS OUTPUT
     O*
     OQSYSPRT   E            HEAD           1 04
     O               63N38   TIT(1)              58
     O              N63N38   TIT(2)              58
     O*** BEGIN ADD ***  10/21/03  ************************************
     O*********           38   TIT,3     58
     O              N30 38   TIT(3)              58
     O               30 38   TIT(4)              58
     O*** END ADD *****  10/21/03  ************************************
     O          E            HEAD           1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HEAD           1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            HEAD           1
     O                       FNMAD1              42
     O**** BEGIN CHG **** 07/02/01 ************************************
     O****************** N38             58 'Policy #:     '
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O**** END   CHG **** 07/02/01 ************************************
     O                  38                       58 'Quote #:      '
     O*** BEGIN CHG ***  10/27/97  ************************************
     O*****           N67      ELFND     71
     O*****           N67                72 '-'
     O*****           N67      ELEMP#    80
     O*****            67      QQPOL#    80
     O                       QQPOL#              80
     O*** END CHG *****  10/27/97  ************************************
     O          E            HEAD           1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            HEAD           1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            HEAD           1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            HEAD           1
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            58 'Agent #:      '
     O*******                  AHAGT#Z   64
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*** END CHG *** 09/24/04 ************************************
     O*** BEGIN CHG ***  09/11/98  ************************************
     O*****   E  1             HEAD
     O*****                    ANAME     80
     O*****   E  1             HEAD
     O*****                    AADD1     80
     O*****   E  1             HEAD
     O*****                    ACSZ      80
     O*****   E  1             HEAD
     O*****            69                47 '('
     O*****            69      AGARE1    50
     O*****            69                51 ')'
     O*****            69      AGPHN1    60 '   -    '
     O*****   E  1             HEAD
     O*****                    EMN,1     53
     O*****   E  1             HEAD
     O*****                    EMN,2     53
     O*****   E  1             HEAD
     O*****                    EMN,3     53
     O*****   E  1             HEAD
     O*****                    EMN,4     53
     O*** BEGIN ADD ***  09/08/98  ************************************
     O*****            66                58 'Carrier Pol#: '
     O*****            66      XXCAP#    84
     O*** END ADD *****  09/08/98  ************************************
     O*****   E  1             HEAD
     O*****                    EMN,5     53
     O          E            HEAD           1
     O                       ANAME               85
     O          E            HEAD           1
     O                       AADD1               85
     O          E            HEAD           1
     O                       EMN(1)              50
     O                       ACSZ                85
     O          E            HEAD           1
     O                       EMN(2)              50
     O               69                          52 '('
     O               69      AGARE1              55
     O               69                          56 ')'
     O               69      AGPHN1              65 '   -    '
     O          E            HEAD           1
     O                       EMN(3)              50
     O          E            HEAD           1
     O                       EMN(4)              50
     O          E            HEAD           1
     O                       EMN(5)              50
     O          E            HEAD           1
     O               66                          58 'Carrier Pol#: '
     O               66      XXCAP#              84
     O          E            HEAD           1
     O*** END CHG *****  09/11/98  ************************************
     O*** BEGIN ADD ***  01/20/98  ************************************
     O                                           58 'Rating State: '
     O                       XXST                61
     O*** END ADD *****  01/20/98  ************************************
     O***  Begin Add  ***  11/19/09  **************************************
     O          E            HEAD           1
     O                       XQSTMH              58
     O                       XQSTMD              84
     O***  End   Add  ***  11/19/09  **************************************
     O*
     O****++ BEGIN ADD +++  01/11/05  +++++++++++++++++++++++++++++++++
     O*
     OQSYSPRT   E            HDG114         1 04
     O               63N38   TIT(1)              58
     O              N63N38   TIT(2)              58
     O              N30 38   TIT(3)              58
     O               30 38   TIT(4)              58
     O          E            HDG114         1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HDG114         1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            HDG114         1
     O                       FNMAD1              42
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O                  38                       58 'Quote #:      '
     O                       QQPOL#              80
     O          E            HDG114         1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            HDG114         1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            HDG114         1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            HDG114         1
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O          E            HDG114         1
     O                       ANAME               85
     O          E            HDG114         1
     O                       AADD1               85
     O          E            HDG114         1
     O                       EMN2(1)             50
     O                       ACSZ                85
     O*
     O          E            HDG114         1
     O                       EMN2(2)             50
     O               69                          52 '('
     O               69      AGARE1              55
     O               69                          56 ')'
     O               69      AGPHN1              65 '   -    '
     O*
     O          E            HDG114         1
     O                       EMN2(3)             50
     O*
     O          E            HDG114         1
     O                       EMN2(4)             50
     O*
     O          E            HDG114         1
     O                       EMN2(5)             50
     O*
     O          E            HDG114         1
     O                       EMN2(6)             50
     O          E            HDG114         1
     O               66                          58 'Carrier Pol#: '
     O               66      XXCAP#              84
     O          E            HDG114         1
     O                                           58 'Rating State: '
     O                       XXST                61
     O****++ END   ADD +++  01/11/05  +++++++++++++++++++++++++++++++++
     **
     O          E            HEAD3          2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HEAD3          1
      **** BEG CHG ****** 04/20/09 ************************************
     O************                               27 'Code    Classification  '
     O                                           14 'Code   Ext '
      **** END CHG ****** 04/20/09 ************************************
      **** BEG ADD ****** 04/20/09 ************************************
     O                                           31 'Classification '
      **** END ADD ****** 04/20/09 ************************************
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********                         62 'Payroll     Rate'
      **** BEG CHG ****** 04/20/09 ************************************
     O************                               64 'Payroll       Rate'
     O                                           69 'Payroll       Rate'
      **** END CHG ****** 04/20/09 ************************************
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O********************************** 75 'Premium'
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*************                              80 'Premium'
     O              N25                          80 'Premium'
     O               25                          80 'Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
      **** END   CHG **** 08/30/01 ************************************
     O          E            HEAD3          2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            DET            1
     O                       EPCLAS        Z      7
     O*** BEGIN CHG ***  11/14/02  ************************************
     O**********               EPSPEC     8
     O                       EPSPEC               9
     O*** END CHG *****  11/14/02  ************************************
     O*** BEG ADD *****  04/20/09  ************************************
     O                       EPEXT               15
     O*** END ADD *****  04/20/09  ************************************
     O*** BEG CHG *****  04/20/09  ************************************
     O**********             PRDESC              41
     O**********             XXEAPR        2     53
     O                       PRDESC              46
     O                       XXEAPR        2     58
     O*** END CHG *****  04/20/09  ************************************
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********               EPARAT2   62
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            EPARAT        2     64
     O                       EPARAT        2     67
     O*** END CHG *****  04/20/09  ************************************
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O************************ XXEMPR1   75
     O                       XXEMPR        1     80
     O*** BEGIN ADD ****** 10/13/11 ************************************
     O                       XXEMSN              81
     O*** END   ADD ****** 10/13/11 ************************************
      **** END   CHG **** 08/30/01 ************************************
     O          E            DET1           1
     O*** BEGIN CHG ***  04/30/03  ************************************
     O*********       N88                12 '('
     O*********       N88      XXFRAVY   22
     O*********       N88                24 '-'
     O*********       N88      XXTOAVY   35
     O*********       N88                36 ')'
     O              N88N97                       12 '('
     O              N88N97   XXFRAV        Y     22
     O              N88N97                       24 '-'
     O              N88N97   XXTOAV        Y     35
     O              N88N97                       36 ')'
     O                  97                       12 '('
     O                  97   XXFRDT        Y     22
     O                  97                       24 '-'
     O                  97   XXTODT        Y     35
     O                  97                       36 ')'
     O*** END CHG *****  04/30/03  ************************************
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********        62                62 'Add''l Load'
     O               62N32                       62 'Add''l Load'
     O               62 32                       62 'Territory Differential'
     O*** END CHG *****  11/14/02  ************************************
     O*** BEG ADD *****  04/26/04  ************************************
     O               62 70                       62 'Charge     '
     O*** END ADD *****  04/26/04  ************************************
     O               62      XXADLD        4     75
     O          E    94      DET2           1
     O                                           26 '93 Rate Credit'
     O                       XXDISC        J     78
     O          E            PAG1           0 64
     O                                           76 'Page'
     O                       XXPAGE        Z     80
     O*** BEGIN ADD ***  09/23/02  ************************************
     O          E            PAG1           0 66
     O                                           85 '.'
     O*** END ADD *****  09/23/02  ************************************
     O*** BEGIN CHG ***  11/12/98  ************************************
     O*****   E  259           BOT
     O          E            BOT            0 59
     O*** END CHG *****  11/12/98  ************************************
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O*** BEGIN ADD ***  11/12/98  ************************************
     O          E            BOT            2
     O              N48                          10 'Standard'
     O              N48                          28 'liability limits:'
     O*** BEGIN CHG ***  10/25/99  ************************************
     O***********     N48      F1EMLB    59
     O              N48      RREMLB              59
     O*** END CHG *****  10/25/99  ************************************
     O*** END ADD *****  11/12/98  ************************************
     O          E            BOT            1
     O                                           14 'Serviced by:'
     O                       XXCONM              80
     O          E            BOT            1
     O                       COM(1)              66
     O*** BEGIN ADD ***  01/20/98  ************************************
     O                       Q1USER              80
     O*** END ADD *****  01/20/98  ************************************
     O          E            BOT            1
     O                       COM(2)              66
     O          E            BOT            1
     O                       COM(3)              66
     O                                           76 ' Page'
     O                       XXPAGE        Z     80
     O*** BEGIN ADD ***  09/23/02  ************************************
     O          E            BOT            0 66
     O                                           85 '.'
     O*** END ADD *****  09/23/02  ************************************
     OQSYSPRT   E            HEAD2          1 02
     O               63N38   TIT(1)              56
     O              N63N38   TIT(2)              56
     O*** BEGIN ADD ***  10/21/03  ************************************
     O*********           38   TIT,3     56
     O              N30 38   TIT(3)              56
     O               30 38   TIT(4)              56
     O*** END ADD *****  10/21/03  ************************************
     O          E            HEAD2          1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HEAD2          1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            HEAD2          1
     O                       FNMAD1              42
     O**** BEGIN CHG **** 07/02/01 ************************************
     O****************** N38             58 'Policy #:     '
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O**** BEGIN CHG **** 07/02/01 ************************************
     O                  38                       58 'Quote #:      '
     O*** BEGIN CHG ***  10/27/97  ************************************
     O*****           N67      ELFND     71
     O*****           N67                72 '-'
     O*****           N67      ELEMP#    80
     O*****            67      QQPOL#    80
     O                       QQPOL#              80
     O*** END CHG *****  10/27/97  ************************************
     O          E            HEAD2          1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            HEAD2          1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            HEAD2          1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            HEAD2          1
     O*** BEGIN CHG ***  09/11/98  ************************************
     O*****                    EMN,1     42
     O*****                              58 'Agent #:      '
     O*****                    AHAGT#Z   64
     O*****   E  1             HEAD2
     O*****                    EMN,2     42
     O*****                    ANAME     80
     O*****   E  1             HEAD2
     O*****                    EMN,3     42
     O*****                    AADD1     80
     O*****   E  1             HEAD2
     O*****                    EMN,4     42
     O*****                    ACSZ      80
     O*****   E  1             HEAD2
     O*****                    EMN,5     42
     O                       EMN(1)              39
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            58 'Agent #:      '
     O*******                  AHAGT#Z   64
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*** END CHG *** 09/24/04 ************************************
     O          E            HEAD2          1
     O                       EMN(2)              39
     O                       ANAME               80
     O          E            HEAD2          1
     O                       EMN(3)              39
     O                       AADD1               80
     O          E            HEAD2          1
     O                       EMN(4)              39
     O                       ACSZ                80
     O          E            HEAD2          1
     O                       EMN(5)              39
     O*** END CHG *****  09/11/98  ************************************
     O               69                          47 '('
     O               69      AGARE1              50
     O               69                          51 ')'
     O               69      AGPHN1              60 '   -    '
     O          E            TOTH           1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E    01      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(1)              33
     O                       PRC(1)              40
     O                       EFD(1)              52 '  /  /    '
     O                       DSH(1)              54
     O                       EXD(1)              65 '  /  /    '
     O                       TAM(1)        2     79
     O                       SGN(1)              80
     O          E            TOTH           0
     O               02                          79 '___________'
     O*** BEGIN CHG ***  04/06/07  **************************************
     O*********       N85                52 '__________'
     O*********       N85                65 '__________'
     O                                           52 '__________'
     O                                           65 '__________'
     O*** END CHG *****  04/06/07  **************************************
     O          E            TOTH           1
     O                       HDG(2)              33
     O                       PRC(2)              40
     O*** BEGIN CHG ***  04/06/07  **************************************
     O*********       N85                52 ' Eff Date '
     O                                           52 ' Eff Date '
     O*** END CHG *****  04/06/07  **************************************
     O                       DSH(2)              54
     O*** BEGIN CHG ***  04/06/07  **************************************
     O*********       N85                65 ' Exp Date '
     O                                           65 ' Exp Date '
     O*** END CHG *****  04/06/07  **************************************
     O                       TAM(2)        2     79
     O                       SGN(2)              80
     O          E            TOTH           0
     O               03                          79 '___________'
     O*** BEGIN DEL ***  04/06/07  **************************************
     O*********        85                52 '__________'
     O*********        85                65 '__________'
     O*** END DEL *****  04/06/07  **************************************
     O          E            TOTH           1
     O                       HDG(3)              33
     O                       PRC(3)              40
     O*** BEGIN CHG ***  04/06/07  **************************************
     O*********        85                52 ' Eff Date '
     O*********       N85      EFD,3     52 '  /  /    '
     O*********       N85      DSH,3     54
     O*********        85                65 ' Exp Date '
     O*********       N85      EXD,3     65 '  /  /    '
     O                       EFD(3)              52 '  /  /    '
     O                       DSH(3)              54
     O                       EXD(3)              65 '  /  /    '
     O*** END CHG *****  04/06/07  **************************************
     O                       TAM(3)        2     79
     O                       SGN(3)              80
     O          E    04      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(4)              33
     O                       PRC(4)              40
     O                       EFD(4)              52 '  /  /    '
     O                       DSH(4)              54
     O                       EXD(4)              65 '  /  /    '
     O                       TAM(4)        2     79
     O                       SGN(4)              80
     O          E    05      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(5)              33
     O                       PRC(5)              40
     O                       EFD(5)              52 '  /  /    '
     O                       DSH(5)              54
     O                       EXD(5)              65 '  /  /    '
     O                       TAM(5)        2     79
     O                       SGN(5)              80
     O          E    06      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(6)              33
     O                       PRC(6)              40
     O                       EFD(6)              52 '  /  /    '
     O                       DSH(6)              54
     O                       EXD(6)              65 '  /  /    '
     O                       TAM(6)        2     79
     O                       SGN(6)              80
     O          E    07      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(7)              33
     O                       PRC(7)              40
     O                       EFD(7)              52 '  /  /    '
     O                       DSH(7)              54
     O                       EXD(7)              65 '  /  /    '
     O                       TAM(7)        2     79
     O                       SGN(7)              80
     O          E    08      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(8)              33
     O                       PRC(8)              40
     O                       EFD(8)              52 '  /  /    '
     O                       DSH(8)              54
     O                       EXD(8)              65 '  /  /    '
     O                       TAM(8)        2     79
     O                       SGN(8)              80
     O          E    09      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(9)              33
     O                       PRC(9)              40
     O                       EFD(9)              52 '  /  /    '
     O                       DSH(9)              54
     O                       EXD(9)              65 '  /  /    '
     O                       TAM(9)        2     79
     O                       SGN(9)              80
     O          E    10      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(10)             33
     O                       PRC(10)             40
     O                       EFD(10)             52 '  /  /    '
     O                       DSH(10)             54
     O                       EXD(10)             65 '  /  /    '
     O                       TAM(10)       2     79
     O                       SGN(10)             80
     O          E    11      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(11)             33
     O                       PRC(11)             40
     O                       EFD(11)             52 '  /  /    '
     O                       DSH(11)             54
     O                       EXD(11)             65 '  /  /    '
     O                       TAM(11)       2     79
     O                       SGN(11)             80
     O          E    12      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(12)             33
     O                       PRC(12)             40
     O                       EFD(12)             52 '  /  /    '
     O                       DSH(12)             54
     O                       EXD(12)             65 '  /  /    '
     O                       TAM(12)       2     79
     O                       SGN(12)             80
     O          E    13      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(13)             33
     O                       PRC(13)             40
     O                       EFD(13)             52 '  /  /    '
     O                       DSH(13)             54
     O                       EXD(13)             65 '  /  /    '
     O                       TAM(13)       2     79
     O                       SGN(13)             80
     O          E    14      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(14)             33
     O                       PRC(14)             40
     O                       EFD(14)             52 '  /  /    '
     O                       DSH(14)             54
     O                       EXD(14)             65 '  /  /    '
     O                       TAM(14)       2     79
     O                       SGN(14)             80
     O          E    15      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(15)             33
     O                       PRC(15)             40
     O                       EFD(15)             52 '  /  /    '
     O                       DSH(15)             54
     O                       EXD(15)             65 '  /  /    '
     O                       TAM(15)       2     79
     O                       SGN(15)             80
     O          E    16      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(16)             33
     O                       PRC(16)             40
     O                       EFD(16)             52 '  /  /    '
     O                       DSH(16)             54
     O                       EXD(16)             65 '  /  /    '
     O                       TAM(16)       2     79
     O                       SGN(16)             80
     O          E    17      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(17)             33
     O                       PRC(17)             40
     O                       EFD(17)             52 '  /  /    '
     O                       DSH(17)             54
     O                       EXD(17)             65 '  /  /    '
     O                       TAM(17)       2     79
     O                       SGN(17)             80
     O          E    18      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(18)             33
     O                       PRC(18)             40
     O                       EFD(18)             52 '  /  /    '
     O                       DSH(18)             54
     O                       EXD(18)             65 '  /  /    '
     O                       TAM(18)       2     79
     O                       SGN(18)             80
     O          E    19      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(19)             33
     O                       PRC(19)             40
     O                       EFD(19)             52 '  /  /    '
     O                       DSH(19)             54
     O                       EXD(19)             65 '  /  /    '
     O                       TAM(19)       2     79
     O                       SGN(19)             80
     O          E    20      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(20)             33
     O                       PRC(20)             40
     O                       EFD(20)             52 '  /  /    '
     O                       DSH(20)             54
     O                       EXD(20)             65 '  /  /    '
     O                       TAM(20)       2     79
     O                       SGN(20)             80
     O          E    21      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(21)             33
     O                       PRC(21)             40
     O                       EFD(21)             52 '  /  /    '
     O                       DSH(21)             54
     O                       EXD(21)             65 '  /  /    '
     O                       TAM(21)       2     79
     O                       SGN(21)             80
     O          E    22      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(22)             33
     O                       PRC(22)             40
     O                       EFD(22)             52 '  /  /    '
     O                       DSH(22)             54
     O                       EXD(22)             65 '  /  /    '
     O                       TAM(22)       2     79
     O                       SGN(22)             80
     O          E    23      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(23)             33
     O                       PRC(23)             40
     O                       EFD(23)             52 '  /  /    '
     O                       DSH(23)             54
     O                       EXD(23)             65 '  /  /    '
     O                       TAM(23)       2     79
     O                       SGN(23)             80
     O          E    24      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(24)             33
     O                       PRC(24)             40
     O                       EFD(24)             52 '  /  /    '
     O                       DSH(24)             54
     O                       EXD(24)             65 '  /  /    '
     O                       TAM(24)       2     79
     O                       SGN(24)             80
     O          E    25      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(25)             33
     O                       PRC(25)             40
     O                       EFD(25)             52 '  /  /    '
     O                       DSH(25)             54
     O                       EXD(25)             65 '  /  /    '
     O                       TAM(25)       2     79
     O                       SGN(25)             80
     O          E    26      TOTH           0
     O                                           79 '___________'
     O          E            TOTH           1
     O                       HDG(26)             33
     O                       PRC(26)             40
     O                       EFD(26)             52 '  /  /    '
     O                       DSH(26)             54
     O                       EXD(26)             65 '  /  /    '
     O                       TAM(26)       2     79
     O                       SGN(26)             80
      *** BEG CHG ***** 07/02/04 *************************************
     O********E  0     27      TOTH
     O********                           79 '___________'
     O********E  1             TOTH
     O********                 HDG,27    33
     O********                 PRC,27    40
     O********                 EFD,27    52 '  /  /    '
     O********                 DSH,27    54
     O********                 EXD,27    65 '  /  /    '
     O********                 TAM,272   79
     O********                 SGN,27    80
     O********E  042           TOTH
     O          E            TOTH           0 41
      *** END CHG ***** 07/02/04 *************************************
     O*** BEGIN CHG ***  02/07/02  ************************************
     O*********        44                47 '* This Policy Has Invoke'
     O*********        44                65 'd Minimum Premium.'
     O               34                          47 '* This Policy Has Invoke'
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O**********     34                          65 'd Minimum Premium.'
     O               34N25                       65 'd Minimum Premium.'
     O               34 25                       65 'd Minimum Contribution.'
     O*** END   CHG ****** 01/14/10 ************************************
     O*** END CHG *****  02/07/02  ************************************
     O          E            TOTH           1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O*
     O*  PRINT TOTALS FOR ESTIMATING ACCOUNTS
     O*
     O          E            TOTE           1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O************   63    39                    25 'Premium               '
     O************  N63    39                    25 'Revised Premium       '
     O               63N25 39                    25 'Premium               '
     O              N63N25 39                    25 'Revised Premium       '
     O               63 25 39                    25 'Contribution          '
     O              N63 25 39                    25 'Revised Contribution  '
     O*** END   CHG ****** 01/14/10 ************************************
     O                  51 39XXPRO         A     43
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      66 'Experience Modifiers:'
     O              N95                          66 'Experience Modifiers:'
     O               95                          66 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O          E            TOTE           1
     O               39 51 52                    25 'Membership Dues       '
     O               39 51 52XXMFDU        A     43
     O                       ELEXM1        2     53
     O                       XXEMD1              65 '  /  /    '
     O          E            TOTE           1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 51 53          25 'Safety Fees Due       '
     O               39 51 53XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 51 53XXSFDU        B     43
     O                       ELEXM2        2     53
     O                       XXEMD2              65 '  /  /    '
     O          E            TOTE           1
     O              N44    39                    25 'Expense Constant      '
     O              N44 51 39XXEXCN        A     43
     O                       ELEXM3        2     53
     O                       XXEMD3              65 '  /  /    '
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTE           1
     O               67    39                    25 'Installment Fees Due  '
     O               67 51 39XXINST        B     43
     O*** END ADD *****  03/04/02  ************************************
     O          E            TOTE           1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45    39          25 'State Tax Due         '
     O              N45    39XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O              N45 51 39XXTXDU        B     43
     O          E       51   TOTE           0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**************         39          41 '___________'
     O                 N57 39                    41 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTE           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      63    39          25 'Premium Paid          '
     O***********     N63    39          25 'Previously Paid       '
     O***********         51 39XXDPRPA   43
     O               63N57 39                    25 'Premium Paid          '
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*************   N63N57 39          25 'Previously Paid       '
     O               92N57 39                    25 'Previously Paid       '
     O               93N57 39                    25 'Previously Billed     '
     O*** END CHG *****  10/13/00  ************************************
     O              N57 51 39XXDPRP        A     43
     O*** END CHG *****  11/15/99  ************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTE           0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          25 'Balance               '
     O************        51 39XXBAL A   43 '$'
     O              N57    39                    25 'Balance               '
     O              N57 51 39XXBAL         A     43 '$'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTE           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**************         39          25 'Balance               '
     O**************      51 39XXBAL A   43 '$'
     O              N57    39                    25 'Balance               '
     O              N57 51 39XXBAL         A     43 '$'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          63 'Deductible Plan:  '
     O              N47      X2DEDT              68
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTE           1
     O*** BEGIN ADD ***  10/13/00  ************************************
     O              N57 37 39                    19 '<Agency Billing>'
     O*** END ADD *****  10/13/00  ************************************
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********       N46      DDAMPO1   56 '$'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O*** END CHG *****  09/14/01  ************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********       N46N47             71 'Per Occurrence'
     O*********       N46 47             71 'Per Claim     '
     O*********       N46N47             76 'Per Occurrence'
     O*********       N46 47             76 'Per Claim     '
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O*** END CHG *****  09/14/01  ************************************
      **** END   CHG **** 08/24/99 ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTE           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********            39          18 'Payment Plan:  '
     O***********            39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*****************  N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O*** END   CHG ***  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********       N46      DDAMAG1   56 '$'
     O*********       N46                73 'Deductible Limit'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O*********       N46      DDAMAG1   61 '$'
     O*********       N46                78 'Deductible Limit'
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  09/14/01  ************************************
     O          E            TOTE           1
     O               71 39   DUW(1)               6
     O*** BEGIN CHG ***  01/25/99  ************************************
     O*****               39   DDU,1     25
     O               63 39   DDU(1)              25
     O*** BEGIN CHG ***  03/18/99  ************************************
     O*****           N63 39             11 'thru'
     O              N63 39 71                    11 'thru'
     O*** END CHG *****  03/18/99  ************************************
     O              N63 39   DDU(1)              30
     O*** END CHG *****  01/25/99  ************************************
     O               71 39 51AMT(1)        B     44
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTE           1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        72 39   DUW,2      6
     O*********           39   DDU,2     25
     O               72 39N86DUW(2)               6
     O                  39N86DDU(2)              25
     O               39N57 86ZZ            2      5
     O*** BEG CHG ***** 07/17/08 **************************************
     O***********      39N57 86          25 'Weekly Installments'
     O*** BEGIN ADD ***  04/25/03  ************************************
     O***********      36N57 86          30 'Installments on 1st/15th'
     O*** END ADD *****  04/25/03  ************************************
     O               39N57 86INSTVB              30
     O*** END CHG ***** 07/17/08 **************************************
     O*** END CHG *****  04/21/03  ************************************
     O               72 39 51AMT(2)        B     44
     O*** BEGIN ADD ***  03/04/02  ************************************
     O               82 39   DUW(12)             48
     O                  39   DDU(12)             67
     O               82 39 51AMT(12)       B     80
     O*** END ADD *****  03/04/02  ************************************
     O          E            TOTE           1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        73 39   DUW,3      6
     O*********           39   DDU,3     25
     O               73 39N86DUW(3)               6
     O                  39N86DDU(3)              25
     O               39N57 86                    11 'Starting'
     O               39N57 86X1WKDT              22 '  /  /    '
     O               39N57 86                    25 'of'
     O*** END CHG *****  04/21/03  ************************************
     O               73 39 51AMT(3)        B     44
     O          E            TOTE           1
     O               74 39   DUW(4)               6
     O                  39   DDU(4)              25
     O               74 39 51AMT(4)        B     44
     O                       BCCOM1              75
     O          E            TOTE           1
     O               75 39   DUW(5)               6
     O                  39   DDU(5)              25
     O               75 39 51AMT(5)        B     44
     O                       BCCOM2              75
     O          E            TOTE           1
     O               76 39   DUW(6)               6
     O                  39   DDU(6)              25
     O               76 39 51AMT(6)        B     44
     O                       BCCOM3              75
     O          E            TOTE           1
     O               77 39   DUW(7)               6
     O                  39   DDU(7)              25
     O               77 39 51AMT(7)        B     44
     O          E            TOTE           1
     O               78 39   DUW(8)               6
     O                  39   DDU(8)              25
     O               78 39 51AMT(8)        B     44
     O          E            TOTE           1
     O               79 39   DUW(9)               6
     O                  39   DDU(9)              25
     O               79 39 51AMT(9)        B     44
     O          E            TOTE           1
     O               80 39   DUW(10)              6
     O                  39   DDU(10)             25
     O               80 39 51AMT(10)       B     44
     O          E            TOTE           1
     O               81 39   DUW(11)              6
     O                  39   DDU(11)             25
     O               81 39 51AMT(11)       B     44
     O*** BEGIN DEL ***  03/04/02  ************************************
     O******  E  1             TOTE
     O******           82 39   DUW,12     6
     O******              39   DDU,12    25
     O******           82 39 51AMT,12B   44
     O*** END DEL *****  03/04/02  ************************************
     O*
     O          E            TOTE1          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O**********     63 39                       25 'Premium               '
     O**********    N63 39                       25 'Revised Premium       '
     O               63 39N25                    25 'Premium               '
     O              N63 39N25                    25 'Revised Premium       '
     O               63 39 25                    25 'Contribution          '
     O              N63 39 25                    25 'Revised Contribution  '
     O*** END   CHG ****** 01/14/10 ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               51 39XXPRO A   43
     O*****                              66 'Experience Modifiers:'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O********            51 39XXPRO A   48
     O********                           71 'Experience Modifiers:'
     O                  51 39XXPRO         A     51
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      74 'Experience Modifiers:'
     O              N95                          74 'Experience Modifiers:'
     O               95                          74 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O               39 51 52                    25 'Membership Dues       '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 52XXMFDUA   43
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O********         39 51 52XXMFDUA   48
     O********                 ELEXM12   58
     O********                 XXEMD1    70 '  /  /    '
     O               39 51 52XXMFDU        A     51
     O                       ELEXM1        2     61
     O                       XXEMD1              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 51 53          25 'Safety Fees Due       '
     O               39 51 53XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 53XXSFDUB   43
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O********         39 51 53XXSFDUB   48
     O********                 ELEXM22   58
     O********                 XXEMD2    70 '  /  /    '
     O               39 51 53XXSFDU        B     51
     O                       ELEXM2        2     61
     O                       XXEMD2              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O               39 51 55XX1STH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 55XX1STBB   43
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      39 51 55XX1STP    32
     O***********      39 51 55XX1STBB   48
     O***********              ELEXM32   58
     O***********              XXEMD3    70 '  /  /    '
     O               39 51 55X196          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              55          35 '%'
     O               39    55                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 55XX1STB        B     51
     O                       ELEXM3        2     61
     O                       XXEMD3              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O               39 51 56XX2NDH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 56XX2NDBB   43
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**********       39 51 56XX2NDP    32
     O**********       39 51 56XX2NDBB   48
     O               39 51 56X296          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              56          35 '%'
     O               39    56                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 56XX2NDB        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            TOTE1          1
     O               39 51 64XX3RDH              23
     O               39 51 64X396          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              64          35 '%'
     O               39    64                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 64XX3RDB        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            TOTE1          1
     O               39 51 91XX4THH              23
     O               39 51 91X496          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              91          35 '%'
     O               39    91                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 91XX4THB        B     51
     O          E            TOTE1          1
     O               39 51 98XX5THH              23
     O               39 51 98X596          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              98          35 '%'
     O               39    98                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 98XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTE1          1
     O               67    39                    25 'Installment Fees Due  '
     O               67 51 39XXINST        B     51
     O*** END ADD *****  03/04/02  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O              N44    39                    25 'Expense Constant      '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****           N44 51 39XXEXCNA   43
     O*****           N46                63 'Deductible Plan:  '
     O*****           N46      XXDEDT    68
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********     N44 51 39XXEXCNA   48
     O***********     N46                68 'Deductible Plan:  '
     O***********     N46      XXDEDT    73
     O              N44 51 39XXEXCN        A     51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46                71 'Deductible Plan:  '
     O**********      N46      XXDEDT    76
     O              N46      XXAMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          78 'Deductible Limit'
     O              N46                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45    39          25 'State Tax Due         '
     O              N45    39XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****           N45 51 39XXTXDUB   43
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*********       N45 51 39XXTXDUB   48
     O              N45 51 39XXTXDU        B     51
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          71 'Deductible Plan:  '
     O              N47      X2DEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O          E       51   TOTE1          0
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  39          41 '___________'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************         39          46 '___________'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*************          39          49 '___________'
     O                 N57 39                    49 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTE1          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*************    63    39          25 'Premium Paid          '
     O*************   N63    39          25 'Previously Paid       '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               51 39XXDPRPA   43
     O*****           N46      DDAMPO1   56 '$'
     O*****           N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********         51 39XXDPRPA   48
     O*************       51 39XXDPRPA   51
     O               63N57 39                    25 'Premium Paid          '
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*************   N63N57 39          25 'Previously Paid       '
     O               92N57 39                    25 'Previously Paid       '
     O               93N57 39                    25 'Previously Billed     '
     O*** END CHG *****  10/13/00  ************************************
     O              N57 51 39XXDPRP        A     51
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O***  END   CHG  ***  04/07/05 *************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                76 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47                          63 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN CHG ***  10/13/00  ************************************
     O******* E  2             TOTE1
     O          E            TOTE1          1
     O*** END CHG *****  10/13/00  ************************************
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***************        39          25 'Balance               '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               51 39XXBAL A   43 '$'
     O*****           N46      DDAMAG1   56 '$'
     O*****           N46                73 'Deductible Limit'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************        51 39XXBAL A   48 '$'
     O***************     51 39XXBAL A   51 '$'
     O              N57    39                    25 'Balance               '
     O              N57 51 39XXBAL         A     51 '$'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD ***  10/13/00  ************************************
     O          E            TOTE1          1
     O              N57 37 39                    19 '<Agency Billing>'
     O*** END ADD *****  10/13/00  ************************************
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTE1          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*****************  N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O               78 39   DUW(8)              48
     O                  39   DDU(8)              67
     O               78 39 51AMT(8)        B     80
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTE1          1
     O               71 39   DUW(1)               6
     O*** BEGIN CHG ***  01/25/99  ************************************
     O*****               39   DDU,1     25
     O               63 39   DDU(1)              25
     O*** BEGIN CHG ***  03/18/99  ************************************
     O*****           N63 39             11 'thru'
     O              N63 39 71                    11 'thru'
     O*** END CHG *****  03/18/99  ************************************
     O              N63 39   DDU(1)              30
     O*** END CHG *****  01/25/99  ************************************
     O               71 39 51AMT(1)        B     44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********        81 39   DUW,11    48
     O*********           39   DDU,11    67
     O*********        81 39 51AMT,11B   80
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*******          80 39   DUW,10    48
     O*******             39   DDU,10    67
     O*******          80 39 51AMT,10B   80
     O               79 39   DUW(9)              48
     O                  39   DDU(9)              67
     O               79 39 51AMT(9)        B     80
     O*** END CHG *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTE1          1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        72 39   DUW,2      6
     O*********           39   DDU,2     25
     O               72 39N86DUW(2)               6
     O                  39N86DDU(2)              25
     O               39N57 86ZZ            2      5
     O*** BEG CHG ***** 07/17/08 **************************************
     O***********      39N57 86          25 'Weekly Installments'
     O*** BEGIN ADD ***  04/25/03  ************************************
     O***********      36N57 86          30 'Installments on 1st/15th'
     O*** END ADD *****  04/25/03  ************************************
     O               39N57 86INSTVB              30
     O*** END CHG ***** 07/17/08 **************************************
     O*** END CHG *****  04/21/03  ************************************
     O               72 39 51AMT(2)        B     44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********        82 39   DUW,12    48
     O*********           39   DDU,12    67
     O*********        82 39 51AMT,12B   80
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*******          81 39   DUW,11    48
     O*******             39   DDU,11    67
     O*******          81 39 51AMT,11B   80
     O               80 39   DUW(10)             48
     O                  39   DDU(10)             67
     O               80 39 51AMT(10)       B     80
     O*** END CHG *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTE1          1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        73 39   DUW,3      6
     O*********           39   DDU,3     25
     O               73 39N86DUW(3)               6
     O                  39N86DDU(3)              25
     O               39N57 86                    11 'Starting'
     O               39N57 86X1WKDT              22 '  /  /    '
     O               39N57 86                    25 'of'
     O*** END CHG *****  04/21/03  ************************************
     O               73 39 51AMT(3)        B     44
     O*** BEGIN ADD ***  01/02/01  ************************************
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*******          82 39   DUW,12    48
     O*******             39   DDU,12    67
     O*******          82 39 51AMT,12B   80
     O               81 39   DUW(11)             48
     O                  39   DDU(11)             67
     O               81 39 51AMT(11)       B     80
     O*** END CHG *****  03/04/02  ************************************
     O*** END ADD *****  01/02/01  ************************************
     O          E            TOTE1          1
     O               74 39   DUW(4)               6
     O                  39   DDU(4)              25
     O               74 39 51AMT(4)        B     44
     O*** BEGIN ADD ***  03/04/02  ************************************
     O               82 39   DUW(12)             48
     O                  39   DDU(12)             67
     O               82 39 51AMT(12)       B     80
     O*** END ADD *****  03/04/02  ************************************
     O*** BEGIN DEL ***  01/02/01  ************************************
     O*********                BCCOM1    75
     O*** END DEL *****  01/02/01  ************************************
     O          E            TOTE1          1
     O               75 39   DUW(5)               6
     O                  39   DDU(5)              25
     O               75 39 51AMT(5)        B     44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********                BCCOM2    75
     O*** BEGIN DEL ***  03/04/02  ************************************
     O*******                  BCCOM1    75
     O*** END DEL *****  03/04/02  ************************************
     O*** BEG ADD *****  07/02/04  ************************************
     O                       BCCOM1              75
     O*** END ADD *****  07/02/04  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTE1          1
     O               76 39   DUW(6)               6
     O                  39   DDU(6)              25
     O               76 39 51AMT(6)        B     44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********                BCCOM3    75
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*******                  BCCOM2    75
     O*** BEG CHG *****  07/02/04  ************************************
     O***********************  BCCOM1    75
     O                       BCCOM2              75
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTE1          1
     O               77 39   DUW(7)               6
     O                  39   DDU(7)              25
     O               77 39 51AMT(7)        B     44
     O*** BEGIN ADD ***  01/02/01  ************************************
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*******                  BCCOM3    75
     O*** BEG CHG *****  07/02/04  ************************************
     O***********************  BCCOM2    75
     O                       BCCOM3              75
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  03/04/02  ************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEG CHG *****  07/02/04  ************************************
     O********E**1             TOTE1
     O***********      78 39   DUW,8      6
     O***********         39   DDU,8     25
     O***********      78 39 51AMT,8 B   44
     O*** END CHG *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O*** BEG DEL *****  07/02/04  ************************************
     O*****************        BCCOM3    75
     O*** END DEL *****  07/02/04  ************************************
     O*** END ADD *****  03/04/02  ************************************
     O*** BEGIN DEL ***  03/04/02  ************************************
     O******* E  1             TOTE1
     O*******          79 39   DUW,9      6
     O*******             39   DDU,9     25
     O*******          79 39 51AMT,9 B   44
     O*** END DEL *****  03/04/02  ************************************
     O*** BEGIN DEL ***  01/02/01  ************************************
     O******  E  1             TOTE1
     O******           80 39   DUW,10     6
     O******              39   DDU,10    25
     O******           80 39 51AMT,10B   44
     O*** END DEL *****  01/02/01  ************************************
     O*
     O*  PRINT TOTALS FOR SELF-REPORTING ACCOUNTS
     O*
     O          E            TOTS           1
     O               39                          23 'Deposit             '
     O               39    51XXDEPA        B     41
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      66 'Experience Modifiers:'
     O              N95                          66 'Experience Modifiers:'
     O               95                          66 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** BEGIN ADD ***  04/12/99  ************************************
     O          E            TOTS           1
     O                 N45 39XXTHD               23
     O               39N45 51XXTXDU        B     41
     O                       ELEXM1        2     53
     O                       XXEMD1              65 '  /  /    '
     O*** END ADD *****  04/12/99  ************************************
     O          E            TOTS           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********       39                23 'Deposit Received    '
     O**********       39    51XXDPRLB   41
     O*** BEG CHG *****  04/08/05  ************************************
     O**********       39N57             23 'Deposit Received    '
     O**********       39N57 51XXDPRLB   41
     O               39                          23 'Deposit Received    '
     O               39    51XXDPRL        B     41
     O*** END CHG *****  04/08/05  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O                       ELEXM2        2     53
     O                       XXEMD2              65 '  /  /    '
     O*** END CHG *****  04/12/99  ************************************
     O          E            TOTS           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************     41 39             23 'Deposit Due *       '
     O************     42 39             23 'Credit *            '
     O************        39 51XXDPDUB   41
     O*** BEG CHG *****  04/08/05  ************************************
     O************     41 39N57          23 'Deposit Due *       '
     O************     42 39N57          23 'Credit *            '
     O************    N57 39 51XXDPDUB   41
     O               41 39                       23 'Deposit Due *       '
     O               42 39                       23 'Credit *            '
     O                  39 51XXDPDU        B     41
     O*** END CHG *****  04/08/05  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O                       ELEXM3        2     53
     O                       XXEMD3              65 '  /  /    '
     O*** END CHG *****  04/12/99  ************************************
     O          E            TOTS           1
     O               39 52                       23 'Membership Dues     '
     O               39 52 51XXMFEE        B     41
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  END   ADD  ***  04/07/05 *************************************
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** END DEL *****  04/12/99  ************************************
     O          E            TOTS           1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 53             23 'Safety Fees Due     '
     O               39 53   XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 53 51XXSFEE        B     41
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     56 '$'
     O              N46      XXOCCL              72
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTS           1
     O               39N35                       23 'Expense Constant Due'
     O               39N35 51XXECDU        B     41
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46                63 'Deductible Plan:  '
     O**********      N46      XXDEDT    68
     O              N46      XXAMAG        1     56 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          73 'Deductible Limit'
     O              N46                          66 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O          E    39    51TOTS           0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********                         39 '___________'
     O              N57                          39 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTS           1
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****            39N45             23 'State Tax Due       '
     O*******            N45 39XXTHD     23
     O*** END CHG *****  05/29/98  ************************************
     O*******          39N45 51XXTXDUBB  41
     O*** END DEL *****  04/12/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    79
     O              N47                          63 'Deductible Plan:  '
     O              N47      X2DEDT              68
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O          E            TOTS           0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      39                23 'Total Due           '
     O***********      39    51XXTLDUB   41 '$'
     O               39N57                       23 'Total Due           '
     O               39N57 51XXTLDU        B     41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTS           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      39                23 'Total Due           '
     O***********      39    51XXTLDUB   41 '$'
     O               39N57                       23 'Total Due           '
     O               39N57 51XXTLDU        B     41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O              N46      DDAMPO        1     56 '$'
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                71 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   56 '$'
     O**********      N46N47             71 'Per Occurrence'
     O**********      N46 47             71 'Per Claim     '
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O          E            TOTS           1
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   56 '$'
     O**********      N46                73 'Deductible Limit'
     O              N47      X2AMPO        1     56 '$'
     O              N47      X2OCCL              72
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTS           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********          N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     56 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          73 'Deductible Limit'
     O              N47                          66 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTS           1
     O               43 72 39                    24 '* Payment Due No Later '
     O               43 72 39                    28 'Than'
     O               43 72 39XXDUED              39 '  /  /    '
     O                  73 39                    24 '* Please Deduct The Cre'
     O                  73 39                    43 'dit Indicated Above'
     O          E            TOTS           2
     O                  73 39                    24 '  From Your First Month'
     O                  73 39                    43 'ly Payroll Report. '
     O          E            TOTS           1
     O                       BCCOM1              75
     O          E            TOTS           1
     O                       BCCOM2              75
     O          E            TOTS           1
     O                       BCCOM3              75
     O*
     O          E            TOTS1          1
     O               39                          23 'Deposit             '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39    51XXDEPAB   41
     O*****                              66 'Experience Modifiers:'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39    51XXDEPAB   48
     O************                       71 'Experience Modifiers:'
     O               39    51XXDEPA        B     51
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      74 'Experience Modifiers:'
     O              N95                          74 'Experience Modifiers:'
     O               95                          74 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD ***  04/12/99  ************************************
     O          E            TOTS1          1
     O                 N45 39XXTHD               23
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      39N45 51XXTXDUB   48
     O***********              ELEXM12   58
     O***********              XXEMD1    70 '  /  /    '
     O               39N45 51XXTXDU        B     51
     O                       ELEXM1        2     61
     O                       XXEMD1              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END ADD *****  04/12/99  ************************************
     O          E            TOTS1          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***************  39                23 'Deposit Received    '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39    51XXDPRLB   41
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39    51XXDPRLB   48
     O***************  39    51XXDPRLB   51
     O               39N57                       23 'Deposit Received    '
     O               39N57 51XXDPRL        B     51
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM12   58
     O*****                    XXEMD1    70 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************           ELEXM22   58
     O**************           XXEMD2    70 '  /  /    '
     O                       ELEXM2        2     61
     O                       XXEMD2              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  04/12/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***************  41 39             23 'Deposit Due *       '
     O***************  42 39             23 'Credit *            '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               39 51XXDPDUB   41
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************       39 51XXDPDUB   48
     O***************     39 51XXDPDUB   51
     O*** BEG CHG *****  04/08/05  ************************************
     O*************    41 39N57          23 'Deposit Due *       '
     O*************    42 39N57          23 'Credit *            '
     O*************   N57 39 51XXDPDUB   51
     O               41 39                       23 'Deposit Due *       '
     O               42 39                       23 'Credit *            '
     O                  39 51XXDPDU        B     51
     O*** END CHG *****  04/08/05  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM22   58
     O*****                    XXEMD2    70 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************           ELEXM32   58
     O**************           XXEMD3    70 '  /  /    '
     O                       ELEXM3        2     61
     O                       XXEMD3              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  04/12/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O               39 52                       23 'Membership Dues     '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 52 51XXMFEEB   41
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 52 51XXMFEEB   48
     O               39 52 51XXMFEE        B     51
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*****                    ELEXM32   58
     O*****                    XXEMD3    70 '  /  /    '
     O*** END DEL *****  04/12/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 53             23 'Safety Fees Due     '
     O               39 53   XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 53 51XXSFEEB   41
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 53 51XXSFEEB   48
     O               39 53 51XXSFEE        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O               39 55   XX1STH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 55 51XX1STBB   41
     O*****           N46                63 'Deductible Plan:  '
     O*****           N46      XXDEDT    68
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 55 51XX1STP    32
     O************     39 55 51XX1STBB   48
     O************    N46                68 'Deductible Plan:  '
     O************    N46      XXDEDT    73
     O               39 55 51X196          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              55          35 '%'
     O               39    55                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 55 51XX1STB        B     51
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O               39 56   XX2NDH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 56 51XX2NDBB   41
     O*****           N46      XXDDD1    79
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 56 51XX2NDP    32
     O************     39 56 51XX2NDBB   48
     O               39 56 51X296          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              56          35 '%'
     O               39    56                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 56 51XX2NDB        B     51
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          71 'Deductible Plan:  '
     O              N47      X2DEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            TOTS1          1
     O               39 64   XX3RDH              23
     O               39 64 51X396          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              64          35 '%'
     O               39    64                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 64 51XX3RDB        B     51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47                          63 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            TOTS1          1
     O               39 91   XX4THH              23
     O               39 91 51X496          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              91          35 '%'
     O               39    91                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 91 51XX4THB        B     51
     O              N46      DDAMPO        1     61 '$'
     O              N46N47                       76 'Per Occurrence'
     O              N46 47                       76 'Per Claim     '
     O          E            TOTS1          1
     O               39 98   XX5THH              23
     O               39 98 51X596          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              98          35 '%'
     O               39    98                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 98 51XX5THB        B     51
     O              N46      DDAMPO        1     61 '$'
     O              N46N47                       76 'Per Occurrence'
     O              N46 47                       76 'Per Claim     '
     O*** END ADD *****  07/02/04  ************************************
     O          E            TOTS1          1
     O               39N35                       23 'Expense Constant Due'
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39N35 51XXECDUB   41
     O*****           N46      DDAMPO1   56 '$'
     O*****           N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39N35 51XXECDUB   48
     O               39N35 51XXECDU        B     51
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*******         N46      DDAMPO1   61 '$'
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                76 'Per Occurrence'
     O*******         N46N47             76 'Per Occurrence'
     O*******         N46 47             76 'Per Claim     '
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  01/02/01  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E    39    51TOTS1          0
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                              39 '___________'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************                     46 '___________'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********                         49 '___________'
     O              N57                          49 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****            39N45             23 'State Tax Due       '
     O*******            N45 39XXTHD     23
     O*** END CHG *****  05/29/98  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39N45 51XXTXDUBB  41
     O*****           N46      DDAMAG1   56 '$'
     O*****           N46                73 'Deductible Limit'
     O*******          39N45 51XXTXDUBB  48
     O*** END DEL *****  04/12/99  ************************************
     O*** BEGIN DEL ***  01/02/01  ************************************
     O******          N46      DDAMAG1   61 '$'
     O******          N46                78 'Deductible Limit'
     O*** END DEL *****  01/02/01  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTS1          2
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      39                23 'Total Due           '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39    51XXTLDUB   41 '$'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************   39    51XXTLDUB   48 '$'
     O***********      39    51XXTLDUB   51 '$'
     O               39N57                       23 'Total Due           '
     O               39N57 51XXTLDU        B     51 '$'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTS1          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********          N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTS1          1
     O               43 72 39                    24 '* Payment Due No Later '
     O               43 72 39                    28 'Than'
     O               43 72 39XXDUED              39 '  /  /    '
     O                  73 39                    24 '* Please Deduct The Cre'
     O                  73 39                    43 'dit Indicated Above'
     O          E            TOTS1          1
     O                  73 39                    24 '  From Your First Month'
     O                  73 39                    43 'ly Payroll Report. '
     O                       BCCOM1              75
     O          E            TOTS1          1
     O                       BCCOM2              75
     O          E            TOTS1          1
     O                       BCCOM3              75
     O*
     O*  PRINT TOTALS FOR AUDITED ACCOUNTS
     O*
     O          E            TOTA           1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*************                              23 'Audited Premium     '
     O              N25                          23 'Audited Premium     '
     O               25                          23 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     41
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      66 'Experience Modifiers:'
     O              N95                          66 'Experience Modifiers:'
     O               95                          66 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O          E            TOTA           1
     O               52                          23 'Membership Dues     '
     O               52    51XXMFEE        B     41
     O                       ELEXM1        2     53
     O                       XXEMD1              65 '  /  /    '
     O          E            TOTA           1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        53                23 'Safety Fees Due     '
     O               53      XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               53    51XXSFEE        B     41
     O                       ELEXM2        2     53
     O                       XXEMD2              65 '  /  /    '
     O          E            TOTA           1
     O                 N35                       23 'Expense Constant    '
     O                 N35 51XXEXCN        B     41
     O                       ELEXM3        2     53
     O                       XXEMD3              65 '  /  /    '
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTA           1
     O               67                          23 'Installment Fees Due'
     O               67 51   XXINST        B     41
     O*** END ADD *****  03/04/02  ************************************
     O***  BEGIN ADD  ***  04/07/05 ********************* ADDED 05/04/07
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  END   ADD  ***  04/07/05 ********************* ADDED 05/04/07
     O          E            TOTA           1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45                23 'State Tax           '
     O                 N45   XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O              N45 51   ELTXDU        A     41
     O*** BEG ADD *****  08/01/05  ************************************
     O          E            TOTA           1
     O                       XXGHDG              23
     O                     51XXGAMT        B     41
     O*** END ADD *****  08/01/05  ************************************
     O          E          51TOTA           0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**************                     39 '___________'
     O              N57                          39 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTA           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**************                     23 'Previously Paid     '
     O**************         51XXDPRLB   41
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*********       N57                23 'Previously Paid     '
     O              N57   N92                    23 'Previously Paid     '
     O              N57    92                    23 'Previously Billed   '
     O*** END CHG *****  10/13/00  ************************************
     O              N57    51XXDPRL        B     41
     O*** END CHG *****  11/15/99  ************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O          E            TOTA           1
     O              N46      XXAMPO        1     56 '$'
     O              N46      XXOCCL              72
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTA           0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********        41                23 'Balance Due *       '
     O*********        42                23 'Credit *            '
     O*********              51XXDPDUB   41 '$'
     O               41N57                       23 'Balance Due *       '
     O               42N57                       23 'Credit *            '
     O                 N57 51XXDPDU        B     41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTA           1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********        41                23 'Balance Due *       '
     O*********        42                23 'Credit *            '
     O*********              51XXDPDUB   41 '$'
     O               41N57                       23 'Balance Due *       '
     O               42N57                       23 'Credit *            '
     O                 N57 51XXDPDU        B     41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    79
     O              N47                          63 'Deductible Plan:  '
     O              N47      X2DEDT              68
     O          E            TOTA           1
     O*** BEGIN ADD *** 10/13/00  ********************* ADDED 05/04/07
     O              N57 37                       19 '<Agency Billing>'
     O*** END ADD ***** 10/13/00  ********************* ADDED 05/04/07
     O***  BEGIN CHG  ***  04/07/05 ******************* ADDED 05/04/07***
     O**********      N46N47             71 'Per Occurrence'
     O**********      N46 47             71 'Per Claim     '
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O          E            TOTA           1
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O***  END   CHG  ***  04/07/05 ******************* ADDED 05/04/07***
     O***  BEGIN CHG  ***  04/07/05 ******************* ADDED 05/04/07***
     O**********      N46      DDAMAG1   56 '$'
     O**********      N46                73 'Deductible Limit'
     O              N47      X2AMPO        1     56 '$'
     O              N47      X2OCCL              72
     O          E            TOTA           1
     O***  END   CHG  ***  04/07/05 ******************* ADDED 05/04/07***
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '* Please Deduct The Cre'
     O******************* 43             43 'dit Indicated Above'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********      N83 43             24 '* Please Deduct The Cre'
     O**********      N83 43             43 'dit Indicated Above'
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     56 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          73 'Deductible Limit'
     O              N47                          66 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTA           2
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '  From Your First Month'
     O******************* 43             43 'ly Payroll Report. '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********      N83 43             24 '  From Your First Month'
     O**********      N83 43             43 'ly Payroll Report. '
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTA           1
     O                       BCCOM1              75
     O          E            TOTA           1
     O                       BCCOM2              75
     O          E            TOTA           1
     O                       BCCOM3              75
     O*** BEG CHG ***** 4/17/06 *************************************
     O          E            TOTA#          1
     O                                           36 'Billed'
     O                                           54 'Paid'
     O                                           78 'Balance Due'
     O          E            TOTA#          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*************                              20 'Audited Premium  '
     O              N25                          20 'Audited Premium  '
     O               25                          20 'Audited Contr.   '
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     40
     O                     51XXPRPD        A     60
     O                     51XXPBAL        A     80
     O          E            TOTA#          1
     O               52                          20 'Membership Dues  '
     O               52    51XXMFEE        B     40
     O               52    51XXMFPD        A     60
     O               52    51XXMBAL        A     80
     O          E            TOTA#          1
     O               53                          20 'Safety Fees Due  '
     O               53    51XXSFEE        B     40
     O               53    51XXSFPD        A     60
     O               53    51XXSBAL        A     80
     O          E            TOTA#          1
     O                 N35                       20 'Expense Constant '
     O                 N35 51XXEXCN        B     40
     O                 N35 51XXEXPD        A     60
     O                 N35 51XXEBAL        A     80
     O*** BEGIN ADD ***  06/05/08  *************************************
     O          E            TOTA#          1
     O                       XXGHDG              23
     O                     51XXGAMT        A     40
     O                     51XXGPD         A     60
     O                     51XXFBAL        A     80
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTA#          1
     O                 N28                       20 'NSF/Legal Fees   '
     O                 N28 51XXGFEE        B     40
     O                 N28 51XXGFPD        A     60
     O                 N28 51XXGBAL        A     80
     O          E            TOTA#          0
     O              N57                          25 '________________________'
     O              N57                          48 '________________________'
     O              N57                          71 '________________________'
     O              N57                          80 '_________'
     O          E          51TOTA#          1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************      N45   XXTHD     20
     O                 N45   XXTHD               23
     O*** END CHG *****  06/05/08  *************************************
     O              N45 51   ELTXDU        A     40
     O              N45 51   XXTAXP        A     60
     O              N45 51   XXTBAL        A     80
     O          E            TOTA#          1
     O              N57                          20 'Total            '
     O              N57      XXTBIL        A     40
     O              N57      XXDPRL        A     60
     O              N57      XXDPDU        A     80
     O          E            TOTA#          1
     O              N57 37                       19 '<Agency Billing>'
     O              N46      DDAMPO        1     56 '$'
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O          E            TOTA#          1
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O              N47      X2AMPO        1     56 '$'
     O              N47      X2OCCL              72
     O          E            TOTA#          1
     O              N57 37                       19 '<Agency Billing>'
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O          E            TOTA#          1
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O          E            TOTA#          2
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O          E            TOTA#          1
     O                                           24 'Experience Modifiers:'
     O          E            TOTA#          1
     O                       ELEXM1        2      9
     O                       XXEMD1              22 '  /  /    '
     O                       BCCOM1              75
     O          E            TOTA#          1
     O                       ELEXM2        2      9
     O                       XXEMD2              22 '  /  /    '
     O                       BCCOM2              75
     O          E            TOTA#          1
     O                       ELEXM3        2      9
     O                       XXEMD3              22 '  /  /    '
     O                       BCCOM3              75
     O          E            TOTA#          1
     O              N46                          19 'Deductible Plan:'
     O              N46      XXDEDT              25
     O              N46      XXDDD1              53
     O          E            TOTA#          1
     O              N46      DDAMPO        1     13 '$'
     O              N46N47                       28 'Per Occurrence'
     O              N46 47                       28 'Per Claim     '
     O          E            TOTA#          1
     O              N46      DDAMAG        1     13 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          30 'Deductible Limit'
     O              N46                          23 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O*
     O          E            TOTA1          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O***************                            23 'Audited Premium     '
     O              N25                          23 'Audited Premium     '
     O               25                          23 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  51XXPRO B   41
     O*****                              66 'Experience Modifiers:'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O****************       51XXPRO B   48
     O****************                   71 'Experience Modifiers:'
     O                     51XXPRO         B     51
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      74 'Experience Modifiers:'
     O              N95                          74 'Experience Modifiers:'
     O               95                          74 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O               52                          23 'Membership Dues     '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            52    51XXMFEEB   41
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     52    51XXMFEEB   48
     O************             ELEXM12   58
     O************             XXEMD1    70 '  /  /    '
     O               52    51XXMFEE        B     51
     O                       ELEXM1        2     61
     O                       XXEMD1              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        53                23 'Safety Fees Due     '
     O               53      XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            53    51XXSFEEB   41
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      53    51XXSFEEB   48
     O***********              ELEXM22   58
     O***********              XXEMD2    70 '  /  /    '
     O               53    51XXSFEE        B     51
     O                       ELEXM2        2     61
     O                       XXEMD2              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O               55      XX1STH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            55    51XX1STBB   41
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     55    51XX1STP    32
     O************     55    51XX1STBB   48
     O************             ELEXM32   58
     O************             XXEMD3    70 '  /  /    '
     O               55    51X196          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              55          35 '%'
     O               39    55                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               55    51XX1STB        B     51
     O                       ELEXM3        2     61
     O                       XXEMD3              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O               56      XX2NDH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            56    51XX2NDBB   41
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    56    51XX2NDP    32
     O*************    56    51XX2NDBB   48
     O               56    51X296          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              56          35 '%'
     O               39    56                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               56    51XX2NDB        B     51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            TOTA1          1
     O               64      XX3RDH              23
     O               64    51X396          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              64          35 '%'
     O               39    64                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               64    51XX3RDB        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            TOTA1          1
     O               91      XX4THH              23
     O               91    51X496          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              91          35 '%'
     O               39    91                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               91    51XX4THB        B     51
     O          E            TOTA1          1
     O               98      XX5THH              23
     O               98    51X596          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              98          35 '%'
     O               39    98                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               98    51XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTA1          1
     O               67                          23 'Installment Fees Due'
     O               67 51   XXINST        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          78 'Deductible Limit'
     O              N46                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  03/04/02  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O                 N35                       23 'Expense Constant    '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****              N35 51XXEXCNB   41
     O*****           N46                63 'Deductible Plan:  '
     O*****           N46      XXDEDT    68
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************       N35 51XXEXCNB   48
     O************    N46                68 'Deductible Plan:  '
     O************    N46      XXDEDT    73
     O                 N35 51XXEXCN        B     51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46                71 'Deductible Plan:  '
     O**********      N46      XXDEDT    76
     O              N47                          71 'Deductible Plan:  '
     O              N47      X2DEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45                23 'State Tax           '
     O                 N45   XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****           N45 51   ELTXDUA   41
     O*****           N46      XXDDD1    79
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********     N45 51   ELTXDUA   48
     O              N45 51   ELTXDU        A     51
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          63 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEG ADD *****  08/01/05  ************************************
     O          E            TOTA1          1
     O                       XXGHDG              23
     O                     51XXGAMT        B     51
     O*** END ADD *****  08/01/05  ************************************
     O          E          51TOTA1          0
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                              39 '___________'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***************                    46 '___________'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********                        49 '___________'
     O              N57                          49 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTA1          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********                        23 'Previously Paid     '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  51XXDPRLB   41
     O*****           N46      DDAMPO1   56 '$'
     O*****           N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************         51XXDPRLB   48
     O***********            51XXDPRLB   51
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*********       N57                23 'Previously Paid     '
     O              N57   N92                    23 'Previously Paid     '
     O              N57    92                    23 'Previously Billed   '
     O*** END CHG *****  10/13/00  ************************************
     O              N57    51XXDPRL        B     51
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O***  END   CHG  ***  04/07/05 *************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                76 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN CHG *** 10/13/00  ************************************
     O*****   E  2             TOTA1
     O          E            TOTA1          1
     O*** END CHG ***** 10/13/00  ************************************
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********        41                23 'Balance Due *       '
     O*********        42                23 'Credit *            '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  51XXDPDUB   41 '$'
     O*****           N46      DDAMAG1   56 '$'
     O*****           N46                73 'Deductible Limit'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************          51XXDPDUB   48 '$'
     O*********              51XXDPDUB   51 '$'
     O               41N57                       23 'Balance Due *       '
     O               42N57                       23 'Credit *            '
     O                 N57 51XXDPDU        B     51 '$'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD *** 10/13/00  ************************************
     O          E            TOTA1          1
     O              N57 37                       19 '<Agency Billing>'
     O*** END ADD ***** 10/13/00  ************************************
     O          E            TOTA1          1
     O*** BEGIN ADD ***  03/04/98  ************************************
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O***************  41                24 '* Payment Due No Later '
     O***************  41                28 'Than'
     O***************  41      XXDUED    39 '  /  /    '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********       41N84             24 '* Payment Due No Later '
     O**********       41N84             28 'Than'
     O**********       41N84   XXDUED    39 '  /  /    '
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O*** END ADD *****  03/04/98  ************************************
     O          E            TOTA1          1
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '* Please Deduct The Cre'
     O******************* 43             43 'dit Indicated Above'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********       N83 43             24 '* Please Deduct The Cre'
     O*********       N83 43             43 'dit Indicated Above'
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTA1          1
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '  From Your First Month'
     O******************* 43             43 'ly Payroll Report. '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********       N83 43             24 '  From Your First Month'
     O*********       N83 43             43 'ly Payroll Report. '
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTA1          1
     O                       BCCOM1              75
     O          E            TOTA1          1
     O                       BCCOM2              75
     O          E            TOTA1          1
     O                       BCCOM3              75
     O*** BEG CHG *****  04/17/06  ************************************
     O          E            TOTA1#         1
     O                                           36 'Billed'
     O                                           54 'Paid'
     O                                           78 'Balance Due'
     O          E            TOTA1#         1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O****************                           20 'Audited Premium  '
     O              N25                          20 'Audited Premium  '
     O               25                          20 'Audited Contr.   '
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     40
     O                     51XXPRPD        A     60
     O                     51XXPBAL        A     80
     O          E            TOTA1#         1
     O               52                          20 'Membership Dues  '
     O               52    51XXMFEE        B     40
     O               52    51XXMFPD        A     60
     O               52    51XXMBAL        A     80
     O          E            TOTA1#         1
     O               53                          20 'Safety Fees Due  '
     O               53    51XXSFEE        B     40
     O               53    51XXSFPD        A     60
     O               53    51XXSBAL        A     80
     O          E            TOTA1#         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************    55      XX1STH    20
     O               55      XX1STH              23
     O*** END CHG *****  06/05/08  *************************************
     O*****MLD         55    51X196  4   30
     O*****MLD         55                31 '%'
     O               55    51XX1STB        B     40
     O               55    51XX1SPD        A     60
     O               55    51XX1BAL        A     80
     O          E            TOTA1#         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************    56      XX2NDH    20
     O               56      XX2NDH              23
     O*** END CHG *****  06/05/08  *************************************
     O*****MLD         56    51X296  4   30
     O*****MLD         56                31 '%'
     O               56    51XX2NDB        B     40
     O               56    51XX2NPD        A     60
     O               56    51XX2BAL        A     80
     O          E            TOTA1#         1
     O                 N35                       20 'Expense Constant '
     O                 N35 51XXEXCN        B     40
     O                 N35 51XXEXPD        A     60
     O                 N35 51XXEBAL        A     80
     O*** BEGIN ADD ***  06/05/08  *************************************
     O          E            TOTA1#         1
     O                       XXGHDG              23
     O                     51XXGAMT        A     40
     O                     51XXGPD         A     60
     O                     51XXFBAL        A     80
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTA1#         1
     O                 N28                       20 'NSF/Legal Fees   '
     O                 N28 51XXGFEE        B     40
     O                 N28 51XXGFPD        A     60
     O                 N28 51XXGBAL        A     80
     O          E            TOTA1#         0
     O              N57                          25 '________________________'
     O              N57                          48 '________________________'
     O              N57                          71 '________________________'
     O              N57                          80 '_________'
     O          E          51TOTA1#         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************      N45   XXTHD     20
     O                 N45   XXTHD               23
     O*** END CHG *****  06/05/08  *************************************
     O              N45 51   ELTXDU        A     40
     O              N45 51   XXTAXP        A     60
     O              N45 51   XXTBAL        A     80
     O          E            TOTA1#         2
     O              N57                          20 'Total            '
     O              N57      XXTBIL        A     40
     O              N57      XXDPRL        A     60
     O              N57      XXDPDU        A     80
     O*** BEGIN DEL ***  06/05/08  *************************************
     O******* E  1             TOTA1#
     O*******         N57 37             19 '<Agency Billing>'
     O*** END DEL *****  06/05/08  *************************************
     O          E            TOTA1#         1
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O*** BEGIN ADD ***  06/05/08  *************************************
     O              N57 37                       80 '<Agency Billing>'
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTA1#         1
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O          E            TOTA1#         1
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O          E            TOTA1#         1
     O                                           24 'Experience Modifiers:'
     O          E            TOTA1#         1
     O                       ELEXM1        2      9
     O                       XXEMD1              22 '  /  /    '
     O                       BCCOM1              75
     O          E            TOTA1#         1
     O                       ELEXM2        2      9
     O                       XXEMD2              22 '  /  /    '
     O                       BCCOM2              75
     O          E            TOTA1#         1
     O                       ELEXM3        2      9
     O                       XXEMD3              22 '  /  /    '
     O                       BCCOM3              75
     O          E            TOTA1#         1
     O              N46                          19 'Deductible Plan:'
     O              N46      XXDEDT              25
     O              N46      XXDDD1              53
     O          E            TOTA1#         1
     O              N46      DDAMPO        1     13 '$'
     O              N46N47                       28 'Per Occurrence'
     O              N46 47                       28 'Per Claim     '
     O          E            TOTA1#         1
     O              N46      DDAMAG        1     13 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          30 'Deductible Limit'
     O              N46                          23 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
      *** END CHG ***** 04/17/06 **************************************
     O*
     O*  PRINT TOTALS THAT ARE COMMON TO BOTH ACCOUNTS.
     O*
     O          E            TOTF           1 64
     O***** beg chg ******* 11/22/13 ******************************************
     O*****                                      37 'Make checks payable to '
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    FNSHNM    67
     O*****                  XXCKNM              67
     O*** END CHG *****  06/01/98  ************************************
     O               28                          37 'Make checks payable to '
     O               28      xxCkNm              67
     O***** end chg ******* 11/22/13 ******************************************
     O                                           76 'Page'
     O                       XXPAGE        Z     80
     O*
     O* QSYSPRT1 - HARDCOPY OUTPUT
     O*
     OQSYSPRT1  E            HEADT          1 04
     O               63N38   TIT(1)              56
     O              N63N38   TIT(2)              56
     O*** BEGIN ADD ***  10/21/03  ************************************
     O*********           38   TIT,3     56
     O              N30 38   TIT(3)              56
     O               30 38   TIT(4)              56
     O*** END ADD *****  10/21/03  ************************************
     O          E            HEADT          1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HEADT          1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            HEADT          1
     O                       FNMAD1              42
     O**** BEGIN CHG **** 07/02/01 ************************************
     O****************** N38             58 'Policy #:     '
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O**** END   CHG **** 07/02/01 ************************************
     O                  38                       58 'Quote #:      '
     O*** BEGIN CHG ***  10/27/97  ************************************
     O*****           N67      ELFND     71
     O*****           N67                72 '-'
     O*****           N67      ELEMP#    80
     O*****            67      QQPOL#    80
     O                       QQPOL#              80
     O*** END CHG *****  10/27/97  ************************************
     O          E            HEADT          1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            HEADT          1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            HEADT          1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            HEADT          1
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            58 'Agent #:      '
     O*******                  AHAGT#Z   64
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*** END CHG *** 09/24/04 ************************************
     O*** BEGIN CHG ***  09/11/98  ************************************
     O*****   E  1             HEADT
     O*****                    ANAME     80
     O*****   E  1             HEADT
     O*****                    AADD1     80
     O*****   E  1             HEADT
     O*****                    ACSZ      80
     O*****   E  1             HEADT
     O*****            69                47 '('
     O*****            69      AGARE1    50
     O*****            69                51 ')'
     O*****            69      AGPHN1    60 '   -    '
     O*****   E  1             HEADT
     O*****                    EMN,1     53
     O*****   E  1             HEADT
     O*****                    EMN,2     53
     O*****   E  1             HEADT
     O*****                    EMN,3     53
     O*****   E  1             HEADT
     O*****                    EMN,4     53
     O*** BEGIN ADD ***  09/08/98  ************************************
     O*****            66                58 'Carrier Pol#: '
     O*****            66      XXCAP#    84
     O*** END ADD *****  09/08/98  ************************************
     O*****   E  1             HEADT
     O*****                    EMN,5     53
     O          E            HEADT          1
     O                       ANAME               85
     O          E            HEADT          1
     O                       AADD1               85
     O          E            HEADT          1
     O                       EMN(1)              50
     O                       ACSZ                85
     O          E            HEADT          1
     O                       EMN(2)              50
     O               69                          52 '('
     O               69      AGARE1              55
     O               69                          56 ')'
     O               69      AGPHN1              65 '   -    '
     O          E            HEADT          1
     O                       EMN(3)              50
     O          E            HEADT          1
     O                       EMN(4)              50
     O          E            HEADT          1
     O                       EMN(5)              50
     O          E            HEADT          1
     O               66                          58 'Carrier Pol#: '
     O               66      XXCAP#              84
     O          E            HEADT          1
     O*** END CHG *****  09/11/98  ************************************
     O*** BEGIN ADD ***  01/20/98  ************************************
     O                                           58 'Rating State: '
     O                       XXST                61
     O*** END ADD *****  01/20/98  ************************************
     O****++ BEGIN ADD +++  01/11/05  +++++++++++++++++++++++++++++++++
     OQSYSPRT1  E            HDT114         1 04
     O               63N38   TIT(1)              56
     O              N63N38   TIT(2)              56
     O              N30 38   TIT(3)              56
     O               30 38   TIT(4)              56
     O*
     O          E            HDT114         1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O*
     O          E            HDT114         1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O*
     O          E            HDT114         1
     O                       FNMAD1              42
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O                  38                       58 'Quote #:      '
     O                       QQPOL#              80
     O          E            HDT114         1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O*
     O          E            HDT114         1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O*
     O          E            HDT114         1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O*
     O          E            HDT114         1
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*
     O          E            HDT114         1
     O                       ANAME               85
     O*
     O          E            HDT114         1
     O                       AADD1               85
     O*
     O          E            HDT114         1
     O                       EMN2(1)             50
     O                       ACSZ                85
     O*
     O          E            HDT114         1
     O                       EMN2(2)             50
     O               69                          52 '('
     O               69      AGARE1              55
     O               69                          56 ')'
     O               69      AGPHN1              65 '   -    '
     O*
     O          E            HDT114         1
     O                       EMN2(3)             50
     O*
     O          E            HDT114         1
     O                       EMN2(4)             50
     O*
     O          E            HDT114         1
     O                       EMN2(5)             50
     O*
     O          E            HDT114         1
     O                       EMN2(6)             50
     O*
     O          E            HDT114         1
     O               66                          58 'Carrier Pol#: '
     O               66      XXCAP#              84
     O          E            HDT114         1
     O                                           58 'Rating State: '
     O                       XXST                61
     O*_______________++++++++ HDT114++++++ ENDS HERE +++_____________
     O****++ END   ADD +++  01/11/05  +++++++++++++++++++++++++++++++++
     O          E            HEAD3T         2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HEAD3T         1
      **** BEG CHG ****** 04/20/09 ************************************
     O*****************                          27 'Code    Classification  '
     O                                           14 'Code   Ext '
      **** END CHG ****** 04/20/09 ************************************
      **** BEG ADD ****** 04/20/09 ************************************
     O                                           31 'Classification '
      **** END ADD ****** 04/20/09 ************************************
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********                         62 'Payroll     Rate'
      **** BEG CHG ****** 04/20/09 ************************************
     O************                               64 'Payroll       Rate'
     O                                           69 'Payroll       Rate'
      **** END CHG ****** 04/20/09 ************************************
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O********************************** 75 'Premium'
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O****************                           80 'Premium'
     O              N25                          80 'Premium'
     O               25                          80 'Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
      **** END   CHG **** 08/30/01 ************************************
     O          E            HEAD3T         2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            DETT           1
     O                       EPCLAS        Z      7
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********                EPSPEC     8
     O                       EPSPEC               9
     O*** END CHG *****  11/14/02  ************************************
     O*** BEG ADD *****  04/20/09  ************************************
     O                       EPEXT               15
     O*** END ADD *****  04/20/09  ************************************
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            PRDESC              41
     O***********            XXEAPR        2     53
     O                       PRDESC              46
     O                       XXEAPR        2     58
     O*** END CHG *****  04/20/09  ************************************
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********               EPARAT2   62
     O*** BEG CHG *****  04/20/09  ************************************
     O**********             EPARAT        2     64
     O                       EPARAT        2     69
     O*** END CHG *****  04/20/09  ************************************
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O************************ XXEMPR1   75
     O                       XXEMPR        1     80
     O*** BEGIN ADD ****** 10/13/11 ************************************
     O                       XXEMSN              81
     O*** END   ADD ****** 10/13/11 ************************************
      **** END   CHG **** 08/30/01 ************************************
     O          E            DET1T          1
     O*** BEGIN CHG ***  04/30/03  ************************************
     O*********       N88                12 '('
     O*********       N88      XXFRAVY   22
     O*********       N88                24 '-'
     O*********       N88      XXTOAVY   35
     O*********       N88                36 ')'
     O              N88N97                       12 '('
     O              N88N97   XXFRAV        Y     22
     O              N88N97                       24 '-'
     O              N88N97   XXTOAV        Y     35
     O              N88N97                       36 ')'
     O                  97                       12 '('
     O                  97   XXFRDT        Y     22
     O                  97                       24 '-'
     O                  97   XXTODT        Y     35
     O                  97                       36 ')'
     O*** END CHG *****  04/30/03  ************************************
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********        62                62 'Add''l Load'
     O               62N32                       62 'Add''l Load'
     O               62 32                       62 'Territory Differential'
     O*** END CHG *****  11/14/02  ************************************
     O*** BEG ADD *****  04/26/04  ************************************
     O               62 70                       62 'Charge     '
     O*** END ADD *****  04/26/04  ************************************
     O               62      XXADLD        4     75
     O          E    94      DET2T          1
     O                                           26 '93 Rate Credit'
     O                       XXDISC        J     78
     O          E            PAG1T          0 64
     O                                           76 'Page'
     O                       XXPAG4        Z     80
     O*** BEGIN CHG ***  11/12/98  ************************************
     O*****   E  259           BOTT
     O          E            BOTT           0 59
     O*** END CHG *****  11/12/98  ************************************
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O*** BEGIN ADD ***  11/12/98  ************************************
     O          E            BOTT           2
     O              N48                          10 'Standard'
     O              N48                          28 'liability limits:'
     O*** BEGIN CHG ***  10/25/99  ************************************
     O***********     N48      F1EMLB    59
     O              N48      RREMLB              59
     O*** END CHG *****  10/25/99  ************************************
     O*** END ADD *****  11/12/98  ************************************
     O          E            BOTT           1
     O                                           14 'Serviced by:'
     O                       XXCONM              80
     O          E            BOTT           1
     O                       COM(1)              66
     O*** BEGIN ADD ***  01/20/98  ************************************
     O                       Q1USER              80
     O*** END ADD *****  01/20/98  ************************************
     O          E            BOTT           1
     O                       COM(2)              66
     O          E            BOTT           1
     O                       COM(3)              66
     O                                           76 ' Page'
     O                       XXPAG4        Z     80
     OQSYSPRT1  E            HEAD2T         1 02
     O               63N38   TIT(1)              56
     O              N63N38   TIT(2)              56
     O*** BEGIN ADD ***  10/21/03  ************************************
     O*********           38   TIT,3     56
     O              N30 38   TIT(3)              56
     O               30 38   TIT(4)              56
     O*** END ADD *****  10/21/03  ************************************
     O          E            HEAD2T         1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            HEAD2T         1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            HEAD2T         1
     O                       FNMAD1              42
     O**** BEGIN CHG **** 07/02/01 ************************************
     O****************** N38             58 'Policy #:     '
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O**** END   CHG **** 07/02/01 ************************************
     O                  38                       58 'Quote #:      '
     O*** BEGIN CHG ***  10/27/97  ************************************
     O*****           N67      ELFND     71
     O*****           N67                72 '-'
     O*****           N67      ELEMP#    80
     O*****            67      QQPOL#    80
     O                       QQPOL#              80
     O*** END CHG *****  10/27/97  ************************************
     O          E            HEAD2T         1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            HEAD2T         1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            HEAD2T         1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            HEAD2T         1
     O*** BEGIN CHG ***  09/11/98  ************************************
     O*****                    EMN,1     42
     O*****                              58 'Agent #:      '
     O*****                    AHAGT#Z   64
     O*****   E  1             HEAD2T
     O*****                    EMN,2     42
     O*****                    ANAME     80
     O*****   E  1             HEAD2T
     O*****                    EMN,3     42
     O*****                    AADD1     80
     O*****   E  1             HEAD2T
     O*****                    EMN,4     42
     O*****                    ACSZ      80
     O*****   E  1             HEAD2T
     O*****                    EMN,5     42
     O                       EMN(1)              39
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            58 'Agent #:      '
     O*******                  AHAGT#Z   64
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*** END CHG *** 09/24/04 ************************************
     O          E            HEAD2T         1
     O                       EMN(2)              39
     O                       ANAME               80
     O          E            HEAD2T         1
     O                       EMN(3)              39
     O                       AADD1               80
     O          E            HEAD2T         1
     O                       EMN(4)              39
     O                       ACSZ                80
     O          E            HEAD2T         1
     O                       EMN(5)              39
     O*** END CHG *****  09/11/98  ************************************
     O               69                          47 '('
     O               69      AGARE1              50
     O               69                          51 ')'
     O               69      AGPHN1              60 '   -    '
     O          E            TOTHT          1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E    01      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(1)              33
     O                       PRC(1)              40
     O                       EFD(1)              52 '  /  /    '
     O                       DSH(1)              54
     O                       EXD(1)              65 '  /  /    '
     O                       TAM(1)        2     79
     O                       SGN(1)              80
     O          E            TOTHT          0
     O               02                          79 '___________'
     O*** BEGIN CHG ***  04/06/07  ***************************************
     O*********       N85                52 '__________'
     O*********       N85                65 '__________'
     O                                           52 '__________'
     O                                           65 '__________'
     O*** END CHG *****  04/06/07  ***************************************
     O          E            TOTHT          1
     O                       HDG(2)              33
     O                       PRC(2)              40
     O*** BEGIN CHG ***  04/06/07  ***************************************
     O*********       N85                52 ' Eff Date '
     O                                           52 ' Eff Date '
     O*** END CHG *****  04/06/07  ***************************************
     O                       DSH(2)              54
     O*** BEGIN CHG ***  04/06/07  ***************************************
     O*********       N85                65 ' Exp Date '
     O                                           65 ' Exp Date '
     O*** END CHG *****  04/06/07  ***************************************
     O                       TAM(2)        2     79
     O                       SGN(2)              80
     O          E            TOTHT          0
     O               03                          79 '___________'
     O*** BEGIN DEL ***  04/06/07  ***************************************
     O*********        85                52 '__________'
     O*********        85                65 '__________'
     O*** END DEL *****  04/06/07  ***************************************
     O          E            TOTHT          1
     O                       HDG(3)              33
     O                       PRC(3)              40
     O*** BEGIN CHG ***  04/06/07  ***************************************
     O*********        85                52 ' Eff Date '
     O*********       N85      EFD,3     52 '  /  /    '
     O*********       N85      DSH,3     54
     O*********        85                65 ' Exp Date '
     O*********       N85      EXD,3     65 '  /  /    '
     O                       EFD(3)              52 '  /  /    '
     O                       DSH(3)              54
     O                       EXD(3)              65 '  /  /    '
     O*** END CHG *****  04/06/07  ***************************************
     O                       TAM(3)        2     79
     O                       SGN(3)              80
     O          E    04      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(4)              33
     O                       PRC(4)              40
     O                       EFD(4)              52 '  /  /    '
     O                       DSH(4)              54
     O                       EXD(4)              65 '  /  /    '
     O                       TAM(4)        2     79
     O                       SGN(4)              80
     O          E    05      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(5)              33
     O                       PRC(5)              40
     O                       EFD(5)              52 '  /  /    '
     O                       DSH(5)              54
     O                       EXD(5)              65 '  /  /    '
     O                       TAM(5)        2     79
     O                       SGN(5)              80
     O          E    06      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(6)              33
     O                       PRC(6)              40
     O                       EFD(6)              52 '  /  /    '
     O                       DSH(6)              54
     O                       EXD(6)              65 '  /  /    '
     O                       TAM(6)        2     79
     O                       SGN(6)              80
     O          E    07      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(7)              33
     O                       PRC(7)              40
     O                       EFD(7)              52 '  /  /    '
     O                       DSH(7)              54
     O                       EXD(7)              65 '  /  /    '
     O                       TAM(7)        2     79
     O                       SGN(7)              80
     O          E    08      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(8)              33
     O                       PRC(8)              40
     O                       EFD(8)              52 '  /  /    '
     O                       DSH(8)              54
     O                       EXD(8)              65 '  /  /    '
     O                       TAM(8)        2     79
     O                       SGN(8)              80
     O          E    09      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(9)              33
     O                       PRC(9)              40
     O                       EFD(9)              52 '  /  /    '
     O                       DSH(9)              54
     O                       EXD(9)              65 '  /  /    '
     O                       TAM(9)        2     79
     O                       SGN(9)              80
     O          E    10      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(10)             33
     O                       PRC(10)             40
     O                       EFD(10)             52 '  /  /    '
     O                       DSH(10)             54
     O                       EXD(10)             65 '  /  /    '
     O                       TAM(10)       2     79
     O                       SGN(10)             80
     O          E    11      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(11)             33
     O                       PRC(11)             40
     O                       EFD(11)             52 '  /  /    '
     O                       DSH(11)             54
     O                       EXD(11)             65 '  /  /    '
     O                       TAM(11)       2     79
     O                       SGN(11)             80
     O          E    12      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(12)             33
     O                       PRC(12)             40
     O                       EFD(12)             52 '  /  /    '
     O                       DSH(12)             54
     O                       EXD(12)             65 '  /  /    '
     O                       TAM(12)       2     79
     O                       SGN(12)             80
     O          E    13      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(13)             33
     O                       PRC(13)             40
     O                       EFD(13)             52 '  /  /    '
     O                       DSH(13)             54
     O                       EXD(13)             65 '  /  /    '
     O                       TAM(13)       2     79
     O                       SGN(13)             80
     O          E    14      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(14)             33
     O                       PRC(14)             40
     O                       EFD(14)             52 '  /  /    '
     O                       DSH(14)             54
     O                       EXD(14)             65 '  /  /    '
     O                       TAM(14)       2     79
     O                       SGN(14)             80
     O          E    15      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(15)             33
     O                       PRC(15)             40
     O                       EFD(15)             52 '  /  /    '
     O                       DSH(15)             54
     O                       EXD(15)             65 '  /  /    '
     O                       TAM(15)       2     79
     O                       SGN(15)             80
     O          E    16      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(16)             33
     O                       PRC(16)             40
     O                       EFD(16)             52 '  /  /    '
     O                       DSH(16)             54
     O                       EXD(16)             65 '  /  /    '
     O                       TAM(16)       2     79
     O                       SGN(16)             80
     O          E    17      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(17)             33
     O                       PRC(17)             40
     O                       EFD(17)             52 '  /  /    '
     O                       DSH(17)             54
     O                       EXD(17)             65 '  /  /    '
     O                       TAM(17)       2     79
     O                       SGN(17)             80
     O          E    18      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(18)             33
     O                       PRC(18)             40
     O                       EFD(18)             52 '  /  /    '
     O                       DSH(18)             54
     O                       EXD(18)             65 '  /  /    '
     O                       TAM(18)       2     79
     O                       SGN(18)             80
     O          E    19      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(19)             33
     O                       PRC(19)             40
     O                       EFD(19)             52 '  /  /    '
     O                       DSH(19)             54
     O                       EXD(19)             65 '  /  /    '
     O                       TAM(19)       2     79
     O                       SGN(19)             80
     O          E    20      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(20)             33
     O                       PRC(20)             40
     O                       EFD(20)             52 '  /  /    '
     O                       DSH(20)             54
     O                       EXD(20)             65 '  /  /    '
     O                       TAM(20)       2     79
     O                       SGN(20)             80
     O          E    21      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(21)             33
     O                       PRC(21)             40
     O                       EFD(21)             52 '  /  /    '
     O                       DSH(21)             54
     O                       EXD(21)             65 '  /  /    '
     O                       TAM(21)       2     79
     O                       SGN(21)             80
     O          E    22      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(22)             33
     O                       PRC(22)             40
     O                       EFD(22)             52 '  /  /    '
     O                       DSH(22)             54
     O                       EXD(22)             65 '  /  /    '
     O                       TAM(22)       2     79
     O                       SGN(22)             80
     O          E    23      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(23)             33
     O                       PRC(23)             40
     O                       EFD(23)             52 '  /  /    '
     O                       DSH(23)             54
     O                       EXD(23)             65 '  /  /    '
     O                       TAM(23)       2     79
     O                       SGN(23)             80
     O          E    24      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(24)             33
     O                       PRC(24)             40
     O                       EFD(24)             52 '  /  /    '
     O                       DSH(24)             54
     O                       EXD(24)             65 '  /  /    '
     O                       TAM(24)       2     79
     O                       SGN(24)             80
     O          E    25      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(25)             33
     O                       PRC(25)             40
     O                       EFD(25)             52 '  /  /    '
     O                       DSH(25)             54
     O                       EXD(25)             65 '  /  /    '
     O                       TAM(25)       2     79
     O                       SGN(25)             80
     O          E    26      TOTHT          0
     O                                           79 '___________'
     O          E            TOTHT          1
     O                       HDG(26)             33
     O                       PRC(26)             40
     O                       EFD(26)             52 '  /  /    '
     O                       DSH(26)             54
     O                       EXD(26)             65 '  /  /    '
     O                       TAM(26)       2     79
     O                       SGN(26)             80
      *** BEG CHG ***** 07/02/04 *************************************
     O********E  0     27      TOTHT
     O********                           79 '___________'
     O********E  1             TOTHT
     O********                 HDG,27    33
     O********                 PRC,27    40
     O********                 EFD,27    52 '  /  /    '
     O********                 DSH,27    54
     O********                 EXD,27    65 '  /  /    '
     O********                 TAM,272   79
     O********                 SGN,27    80
     O********E  042           TOTHT
     O          E            TOTHT          0 41
      *** END CHG ***** 07/02/04 *************************************
     O*** BEGIN CHG ***  02/07/02  ************************************
     O*********        44                47 '* This Policy Has Invoke'
     O*********        44                65 'd Minimum Premium.'
     O               34                          47 '* This Policy Has Invoke'
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O**********     34                          65 'd Minimum Premium.'
     O               34N25                       65 'd Minimum Premium.'
     O               34 25                       65 'd Minimum Contribution.'
     O*** END   CHG ****** 01/14/10 ************************************
     O*** END CHG *****  02/07/02  ************************************
     O          E            TOTHT          1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O*
     O*  PRINT TOTALS FOR ESTIMATING ACCOUNTS
     O*
     O          E            TOTET          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*********      63    39                    25 'Premium               '
     O*********     N63    39                    25 'Revised Premium       '
     O               63N25 39                    25 'Premium               '
     O              N63N25 39                    25 'Revised Premium       '
     O               63 25 39                    25 'Contribution          '
     O              N63 25 39                    25 'Revised Contribution  '
     O*** END   CHG ****** 01/14/10 ************************************
     O                  51 39XXPRO         AB    43
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      66 'Experience Modifiers:'
     O              N95                          66 'Experience Modifiers:'
     O               95                          66 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O          E            TOTET          1
     O               39 51 52                    25 'Membership Dues       '
     O               39 51 52XXMFDU        AB    43
     O                       ELEXM1        2     53
     O                       XXEMD1              65 '  /  /    '
     O          E            TOTET          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 51 53          25 'Safety Fees Due       '
     O               39 51 53XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 51 53XXSFDU        BB    43
     O                       ELEXM2        2     53
     O                       XXEMD2              65 '  /  /    '
     O          E            TOTET          1
     O              N44    39                    25 'Expense Constant      '
     O              N44 51 39XXEXCN        AB    43
     O                       ELEXM3        2     53
     O                       XXEMD3              65 '  /  /    '
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTET          1
     O               67    39                    25 'Installment Fees Due  '
     O               67 51 39XXINST        BB    43
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  03/04/02  ************************************
     O          E            TOTET          1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45    39          25 'State Tax Due         '
     O                 N45 39XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O              N45 51 39XXTXDU        BB    43
     O          E       51   TOTET          0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********            39          41 '___________'
     O                 N57 39                    41 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTET          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      63    39          25 'Premium Paid          '
     O***********     N63    39          25 'Previously Paid       '
     O***********         51 39XXDPRPAB  43
     O               63N57 39                    25 'Premium Paid          '
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*************   N63N57 39          25 'Previously Paid       '
     O               92N57 39                    25 'Previously Paid       '
     O               93N57 39                    25 'Previously Billed     '
     O*** END CHG *****  10/13/00  ************************************
     O              N57 51 39XXDPRP        AB    43
     O*** END CHG *****  11/15/99  ************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O          E            TOTET          1
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTET          0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********             39          25 'Balance               '
     O**********          51 39XXBAL A   43 '$'
     O              N57    39                    25 'Balance               '
     O              N57 51 39XXBAL         A     43 '$'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTET          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********             39          25 'Balance               '
     O**********          51 39XXBAL AB  43 '$'
     O              N57    39                    25 'Balance               '
     O              N57 51 39XXBAL         AB    43 '$'
     O*** END CHG *****  11/15/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          63 'Deductible Plan:  '
     O              N47      X2DEDT              68
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTET          1
     O*** BEGIN ADD ***  10/13/00  ************************************
     O              N57 37 39                    19 '<Agency Billing>'
     O*** END ADD *****  10/13/00  ************************************
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********       N46      DDAMPO1   56 '$'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  09/14/01  ************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********       N46N47             71 'Per Occurrence'
     O*********       N46 47             71 'Per Claim     '
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  09/14/01  ************************************
      **** END   CHG **** 08/24/99 ************************************
     O          E            TOTET          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********          N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********       N46      DDAMAG1   56 '$'
     O*********       N46                73 'Deductible Limit'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  09/14/01  ************************************
     O          E            TOTET          1
     O               71 39   DUW(1)         B     6
     O*** BEGIN CHG ***  01/25/99  ************************************
     O*****               39   DDU,1  B  25
     O               63 39   DDU(1)         B    25
     O*** BEGIN CHG ***  03/18/99  ************************************
     O*****           N63 39             11 'thru'
     O              N63 39 71                    11 'thru'
     O*** END CHG *****  03/18/99  ************************************
     O              N63 39   DDU(1)         B    30
     O*** END CHG *****  01/25/99  ************************************
     O               71 39 51AMT(1)        BB    44
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTET          1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        72 39   DUW,2      6
     O*********           39   DDU,2     25
     O               72 39N86DUW(2)               6
     O                  39N86DDU(2)              25
     O               39N57 86ZZ            2      5
     O*** BEG CHG ***** 07/17/08 **************************************
     O***********      39N57 86          25 'Weekly Installments'
     O*** BEGIN ADD ***  04/25/03  ************************************
     O***********      36N57 86          30 'Installments on 1st/15th'
     O*** END ADD *****  04/25/03  ************************************
     O               39N57 86INSTVB              30
     O*** END CHG ***** 07/17/08 **************************************
     O*** END CHG *****  04/21/03  ************************************
     O               72 39 51AMT(2)        BB    44
     O*** BEGIN ADD ***  03/04/02  ************************************
     O               82 39   DUW(12)        B    48
     O                  39   DDU(12)        B    67
     O               82 39 51AMT(12)       BB    80
     O*** END ADD *****  03/04/02  ************************************
     O          E            TOTET          1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        73 39   DUW,3      6
     O*********           39   DDU,3     25
     O               73 39N86DUW(3)               6
     O                  39N86DDU(3)              25
     O               39N57 86                    11 'Starting'
     O               39N57 86X1WKDT              22 '  /  /    '
     O               39N57 86                    25 'of'
     O*** END CHG *****  04/21/03  ************************************
     O               73 39 51AMT(3)        BB    44
     O          E            TOTET          1
     O               74 39   DUW(4)         B     6
     O                  39   DDU(4)         B    25
     O               74 39 51AMT(4)        BB    44
     O                       BCCOM1         B    75
     O          E            TOTET          1
     O               75 39   DUW(5)         B     6
     O                  39   DDU(5)         B    25
     O               75 39 51AMT(5)        BB    44
     O                       BCCOM2         B    75
     O          E            TOTET          1
     O               76 39   DUW(6)         B     6
     O                  39   DDU(6)         B    25
     O               76 39 51AMT(6)        BB    44
     O                       BCCOM3         B    75
     O          E            TOTET          1
     O               77 39   DUW(7)         B     6
     O                  39   DDU(7)         B    25
     O               77 39 51AMT(7)        BB    44
     O          E            TOTET          1
     O               78 39   DUW(8)         B     6
     O                  39   DDU(8)         B    25
     O               78 39 51AMT(8)        BB    44
     O          E            TOTET          1
     O               79 39   DUW(9)         B     6
     O                  39   DDU(9)         B    25
     O               79 39 51AMT(9)        BB    44
     O          E            TOTET          1
     O               80 39   DUW(10)        B     6
     O                  39   DDU(10)        B    25
     O               80 39 51AMT(10)       BB    44
     O          E            TOTET          1
     O               81 39   DUW(11)        B     6
     O                  39   DDU(11)        B    25
     O               81 39 51AMT(11)       BB    44
     O*** BEGIN DEL ***  03/04/02  ************************************
     O******  E  1             TOTET
     O******           82 39   DUW,12 B   6
     O******              39   DDU,12 B  25
     O******           82 39 51AMT,12BB  44
     O*** END DEL *****  03/04/02  ************************************
     O*
     O          E            TOTET1         1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O***********    63    39                    25 'Premium               '
     O***********   N63    39                    25 'Revised Premium       '
     O               63N25 39                    25 'Premium               '
     O              N63N25 39                    25 'Revised Premium       '
     O               63 25 39                    25 'Contribution          '
     O              N63 25 39                    25 'Revised Contribution  '
     O*** END   CHG ****** 01/14/10 ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               51 39XXPRO AB  43
     O*****                              66 'Experience Modifiers:'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********         51 39XXPRO AB  48
     O***********                        71 'Experience Modifiers:'
     O                  51 39XXPRO         AB    51
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      74 'Experience Modifiers:'
     O              N95                          74 'Experience Modifiers:'
     O               95                          74 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O               39 51 52                    25 'Membership Dues       '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 52XXMFDUAB  43
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      39 51 52XXMFDUAB  48
     O***********              ELEXM12   58
     O***********              XXEMD1    70 '  /  /    '
     O               39 51 52XXMFDU        AB    51
     O                       ELEXM1        2     61
     O                       XXEMD1              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 51 53          25 'Safety Fees Due       '
     O               39 51 53XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 53XXSFDUBB  43
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      39 51 53XXSFDUBB  48
     O***********              ELEXM22   58
     O***********              XXEMD2    70 '  /  /    '
     O               39 51 53XXSFDU        BB    51
     O                       ELEXM2        2     61
     O                       XXEMD2              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O               39 51 55XX1STH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 55XX1STBBB  43
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      39 51 55XX1STP    32
     O***********      39 51 55XX1STBBB  48
     O***********              ELEXM32   58
     O***********              XXEMD3    70 '  /  /    '
     O               39 51 55X196          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              55          35 '%'
     O               39    55                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 55XX1STB        BB    51
     O                       ELEXM3        2     61
     O                       XXEMD3              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O               39 51 56XX2NDH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 51 56XX2NDBBB  43
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**********       39 51 56XX2NDP    32
     O**********       39 51 56XX2NDBBB  48
     O               39 51 56X296          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              56          35 '%'
     O               39    56                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 56XX2NDB        BB    51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            TOTET1         1
     O               39 51 64XX3RDH              23
     O               39 51 64X396          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              64          35 '%'
     O               39    64                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 64XX3RDB        BB    51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            TOTET1         1
     O               39 51 91XX4THH              23
     O               39 51 91X496          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              91          35 '%'
     O               39    91                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 91XX4THB        B     51
     O          E            TOTET1         1
     O               39 51 98XX5THH              23
     O               39 51 98X596          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              98          35 '%'
     O               39    98                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 51 98XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTET1         1
     O               67    39                    25 'Installment Fees Due  '
     O               67 51 39XXINST        BB    51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          78 'Deductible Limit'
     O              N46                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  03/04/02  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O              N44    39                    25 'Expense Constant      '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****           N44 51 39XXEXCNAB  43
     O*****           N46                63 'Deductible Plan:  '
     O*****           N46      XXDEDT    68
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**********      N44 51 39XXEXCNAB  48
     O**********      N46                68 'Deductible Plan:  '
     O**********      N46      XXDEDT    73
     O              N44 51 39XXEXCN        AB    51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46                71 'Deductible Plan:  '
     O**********      N46      XXDEDT    76
     O              N47                          71 'Deductible Plan:  '
     O              N47      X2DEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45    39          25 'State Tax Due         '
     O                 N45 39XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****           N45 51 39XXTXDUBB  43
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********     N45 51 39XXTXDUBB  48
     O              N45 51 39XXTXDU        BB    51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          63 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O              N46      XXDDD1              80
     O          E       51   TOTET1         0
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  39          41 '___________'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************          39          46 '___________'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********             39          49 '___________'
     O              N57    39                    49 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTET1         1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********        63    39          25 'Premium Paid          '
     O*********       N63    39          25 'Previously Paid       '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               51 39XXDPRPAB  43
     O*****           N46      DDAMPO1   56 '$'
     O*****           N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************        51 39XXDPRPAB  48
     O*********           51 39XXDPRPAB  51
     O               63N57 39                    25 'Premium Paid          '
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*************   N63N57 39          25 'Previously Paid       '
     O               92N57 39                    25 'Previously Paid       '
     O               93N57 39                    25 'Previously Billed     '
     O*** END CHG *****  10/13/00  ************************************
     O              N57 51 39XXDPRP        AB    51
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O***  END   CHG  ***  04/07/05 *************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                76 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN CHG ***  10/13/00  ************************************
     O******* E  2             TOTET1
     O          E            TOTET1         1
     O*** END CHG *****  10/13/00  ************************************
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********             39          25 'Balance               '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               51 39XXBAL AB  43 '$'
     O*****           N46      DDAMAG1   56 '$'
     O*****           N46                73 'Deductible Limit'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O**************      51 39XXBAL AB  48 '$'
     O**********          51 39XXBAL AB  51 '$'
     O              N57    39                    25 'Balance               '
     O              N57 51 39XXBAL         AB    51 '$'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD ***  10/13/00  ************************************
     O          E            TOTET1         1
     O              N57 37 39                    19 '<Agency Billing>'
     O*** END ADD *****  10/13/00  ************************************
     O          E            TOTET1         1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********          N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O               78 39   DUW(8)              48
     O                  39   DDU(8)              67
     O               78 39 51AMT(8)        BB    80
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTET1         1
     O               71 39   DUW(1)               6
     O*** BEGIN CHG ***  01/25/99  ************************************
     O*****               39   DDU,1     25
     O               63 39   DDU(1)              25
     O*** BEGIN CHG ***  03/18/99  ************************************
     O*****           N63 39             11 'thru'
     O              N63 39 71                    11 'thru'
     O*** END CHG *****  03/18/99  ************************************
     O              N63 39   DDU(1)              30
     O*** END CHG *****  01/25/99  ************************************
     O               71 39 51AMT(1)        BB    44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********        81 39   DUW,11    48
     O*********           39   DDU,11    67
     O*********        81 39 51AMT,11BB  80
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*********        80 39   DUW,10    48
     O*********           39   DDU,10    67
     O*********        80 39 51AMT,10BB  80
     O               79 39   DUW(9)              48
     O                  39   DDU(9)              67
     O               79 39 51AMT(9)        BB    80
     O*** END CHG *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTET1         1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        72 39   DUW,2      6
     O*********           39   DDU,2     25
     O               72 39N86DUW(2)               6
     O                  39N86DDU(2)              25
     O               39N57 86ZZ            2      5
     O*** BEG CHG ***** 07/17/08 **************************************
     O***********      39N57 86          25 'Weekly Installments'
     O*** BEGIN ADD ***  04/25/03  ************************************
     O***********      36N57 86          30 'Installments on 1st/15th'
     O*** END ADD *****  04/25/03  ************************************
     O               39N57 86INSTVB              30
     O*** END CHG ***** 07/17/08 **************************************
     O*** END CHG *****  04/21/03  ************************************
     O               72 39 51AMT(2)        BB    44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********        82 39   DUW,12    48
     O*********           39   DDU,12    67
     O*********        82 39 51AMT,12BB  80
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*********        81 39   DUW,11    48
     O*********           39   DDU,11    67
     O*********        81 39 51AMT,11BB  80
     O               80 39   DUW(10)             48
     O                  39   DDU(10)             67
     O               80 39 51AMT(10)       BB    80
     O*** END CHG *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTET1         1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        73 39   DUW,3      6
     O*********           39   DDU,3     25
     O               73 39N86DUW(3)               6
     O                  39N86DDU(3)              25
     O               39N57 86                    11 'Starting'
     O               39N57 86X1WKDT              22 '  /  /    '
     O               39N57 86                    25 'of'
     O*** END CHG *****  04/21/03  ************************************
     O               73 39 51AMT(3)        BB    44
     O*** BEGIN ADD ***  01/02/01  ************************************
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*********        82 39   DUW,12    48
     O*********           39   DDU,12    67
     O*********        82 39 51AMT,12BB  80
     O               81 39   DUW(11)             48
     O                  39   DDU(11)             67
     O               81 39 51AMT(11)       BB    80
     O*** END CHG *****  03/04/02  ************************************
     O*** END ADD *****  01/02/01  ************************************
     O          E            TOTET1         1
     O               74 39   DUW(4)               6
     O                  39   DDU(4)              25
     O               74 39 51AMT(4)        BB    44
     O*** BEGIN ADD ***  03/04/02  ************************************
     O               82 39   DUW(12)             48
     O                  39   DDU(12)             67
     O               82 39 51AMT(12)       BB    80
     O*** END ADD *****  03/04/02  ************************************
     O*** BEGIN DEL ***  01/02/01  ************************************
     O*********                BCCOM1    75
     O*** END DEL *****  01/02/01  ************************************
     O          E            TOTET1         1
     O               75 39   DUW(5)               6
     O                  39   DDU(5)              25
     O               75 39 51AMT(5)        BB    44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********                BCCOM2    75
     O*** BEGIN DEL ***  03/04/02  ************************************
     O*********                BCCOM1    75
     O*** END DEL *****  03/04/02  ************************************
     O*** BEG ADD *****  07/02/04  ************************************
     O                       BCCOM1              75
     O*** END ADD *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTET1         1
     O               76 39   DUW(6)               6
     O                  39   DDU(6)              25
     O               76 39 51AMT(6)        BB    44
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*********                BCCOM3    75
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*********                BCCOM2    75
      *** BEG CHG ***** 07/02/04 **************************************
     O*****************        BCCOM1    75
     O                       BCCOM2              75
      *** END CHG ***** 07/02/04 **************************************
     O*** END CHG *****  03/04/02  ************************************
     O*** END CHG *****  01/02/01  ************************************
     O          E            TOTET1         1
     O               77 39   DUW(7)               6
     O                  39   DDU(7)              25
     O               77 39 51AMT(7)        BB    44
     O*** BEGIN ADD ***  01/02/01  ************************************
     O*** BEGIN CHG ***  03/04/02  ************************************
     O*********                BCCOM3    75
      *** BEG CHG ***** 07/02/04 **************************************
     O*****************        BCCOM2    75
     O                       BCCOM3              75
      *** END CHG ***** 07/02/04 **************************************
     O*** END CHG *****  03/04/02  ************************************
     O*** END ADD *****  01/02/01  ************************************
      *** BEG DEL ***** 07/02/04 **************************************
     O********E**1**           TOTET1
     O**************   78 39   DUW,8      6
     O**************      39   DDU,8     25
     O**************   78 39 51AMT,8 BB  44
     O****BEGIN*ADD****  03/04/02  ************************************
     O**************           BCCOM3    75
      *** END DEL ***** 07/02/04 **************************************
     O*** END ADD *****  03/04/02  ************************************
     O*** BEGIN DEL ***  03/04/02  ************************************
     O******* E  1             TOTET1
     O*******          79 39   DUW,9      6
     O*******             39   DDU,9     25
     O*******          79 39 51AMT,9 BB  44
     O*** END DEL *****  03/04/02  ************************************
     O*** BEGIN DEL ***  01/02/01  ************************************
     O******* E  1             TOTET1
     O*******          80 39   DUW,10     6
     O*******             39   DDU,10    25
     O*******          80 39 51AMT,10BB  44
     O*** END DEL *****  01/02/01  ************************************
     O*
     O*  PRINT TOTALS FOR SELF-REPORTING ACCOUNTS
     O*
     O          E            TOTST          1
     O               39                          23 'Deposit             '
     O               39    51XXDEPA        BB    41
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      66 'Experience Modifiers:'
     O              N95                          66 'Experience Modifiers:'
     O               95                          66 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** BEGIN ADD ***  04/12/99  ************************************
     O          E            TOTST          1
     O                 N45 39XXTHD               23
     O               39N45 51XXTXDU        BB    41
     O                       ELEXM1        2     53
     O                       XXEMD1              65 '  /  /    '
     O*** END ADD *****  04/12/99  ************************************
     O          E            TOTST          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      39                23 'Deposit Received    '
     O***********      39    51XXDPRLBB  41
      ** BEG CHG ***** 04/08/05 ***************************************
     O***********      39N57             23 'Deposit Received    '
     O***********      39N57 51XXDPRLBB  41
     O               39                          23 'Deposit Received    '
     O               39    51XXDPRL        BB    41
      ** END CHG ***** 04/08/05 ***************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O                       ELEXM2        2     53
     O                       XXEMD2              65 '  /  /    '
     O*** END CHG *****  04/12/99  ************************************
     O          E            TOTST          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********       41 39             23 'Deposit Due *       '
     O**********       42 39             23 'Credit *            '
     O**********          39 51XXDPDUBB  41
      ** BEG CHG ***** 04/08/05 ***************************************
     O**********       41 39N57          23 'Deposit Due *       '
     O**********       42 39N57          23 'Credit *            '
     O**********      N57 39 51XXDPDUBB  41
     O               41 39                       23 'Deposit Due *       '
     O               42 39                       23 'Credit *            '
     O                  39 51XXDPDU        BB    41
      ** END CHG ***** 04/08/05 ***************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O                       ELEXM3        2     53
     O                       XXEMD3              65 '  /  /    '
     O*** END CHG *****  04/12/99  ************************************
     O          E            TOTST          1
     O               39 52                       23 'Membership Dues     '
     O               39 52 51XXMFEE        BB    41
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  END   ADD  ***  04/07/05 *************************************
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** END DEL *****  04/12/99  ************************************
     O          E            TOTST          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 53             23 'Safety Fees Due     '
     O               39 53   XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 53 51XXSFEE        BB    41
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     56 '$'
     O              N46      XXOCCL              72
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTST          1
     O               39N35                       23 'Expense Constant Due'
     O               39N35 51XXECDU        BB    41
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46                63 'Deductible Plan:  '
     O**********      N46      XXDEDT    68
     O              N46      XXAMAG        1     56 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          73 'Deductible Limit'
     O              N46                          66 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O          E    39    51TOTST          0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************                       39 '___________'
     O              N57                          39 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTST          1
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****            39N45             23 'State Tax Due       '
     O*******            N45 39XXTHD     23
     O*** END CHG *****  05/29/98  ************************************
     O*******          39N45 51XXTXDUBB  41
     O*** END DEL *****  04/12/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    79
     O              N47                          63 'Deductible Plan:  '
     O              N47      X2DEDT              68
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTST          0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************     39                23 'Total Due           '
     O************     39    51XXTLDUBB  41 '$'
     O               39N57                       23 'Total Due           '
     O               39N57 51XXTLDU        BB    41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTST          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********       39                23 'Total Due           '
     O**********       39    51XXTLDUBB  41 '$'
     O               39N57                       23 'Total Due           '
     O               39N57 51XXTLDU        BB    41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   56 '$'
     O***  END   CHG  ***  04/07/05 *************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                71 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             71 'Per Occurrence'
     O**********      N46 47             71 'Per Claim     '
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O          E            TOTST          1
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   56 '$'
     O**********      N46                73 'Deductible Limit'
     O              N47      X2AMPO        1     56 '$'
     O              N47      X2OCCL              72
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTST          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********          N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     56 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          73 'Deductible Limit'
     O              N47                          66 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTST          1
     O               43 72 39                    24 '* Payment Due No Later '
     O               43 72 39                    28 'Than'
     O               43 72 39XXDUED              39 '  /  /    '
     O                  73 39                    24 '* Please Deduct The Cre'
     O                  73 39                    43 'dit Indicated Above'
     O          E            TOTST          2
     O                  73 39                    24 '  From Your First Month'
     O                  73 39                    43 'ly Payroll Report. '
     O          E            TOTST          1
     O                       BCCOM1         B    75
     O          E            TOTST          1
     O                       BCCOM2         B    75
     O          E            TOTST          1
     O                       BCCOM3         B    75
     O*
     O          E            TOTST1         1
     O               39                          23 'Deposit             '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39    51XXDEPABB  41
     O*****                              66 'Experience Modifiers:'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39    51XXDEPABB  48
     O************                       71 'Experience Modifiers:'
     O               39    51XXDEPA        BB    51
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      74 'Experience Modifiers:'
     O              N95                          74 'Experience Modifiers:'
     O               95                          74 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD ***  04/12/99  ************************************
     O          E            TOTST1         1
     O                 N45 39XXTHD               23
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39N45 51XXTXDUBB  48
     O*************            ELEXM12   58
     O*************            XXEMD1    70 '  /  /    '
     O               39N45 51XXTXDU        BB    51
     O                       ELEXM1        2     61
     O                       XXEMD1              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END ADD *****  04/12/99  ************************************
     O          E            TOTST1         1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********        39                23 'Deposit Received    '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39    51XXDPRLBB  41
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39    51XXDPRLBB  48
     O*********        39    51XXDPRLBB  51
      ** BEG CHG ***** 04/08/05 ***************************************
     O*********        39N57             23 'Deposit Received    '
     O*********        39N57 51XXDPRLBB  51
     O               39                          23 'Deposit Received    '
     O               39    51XXDPRL        BB    51
      ** END CHG ***** 04/08/05 ***************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM12   58
     O*****                    XXEMD1    70 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************            ELEXM22   58
     O*************            XXEMD2    70 '  /  /    '
     O                       ELEXM2        2     61
     O                       XXEMD2              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  04/12/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTST1         1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O********         41 39             23 'Deposit Due *       '
     O********         42 39             23 'Credit *            '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****               39 51XXDPDUBB  41
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************       39 51XXDPDUBB  48
     O********            39 51XXDPDUBB  51
      ** BEG CHG ***** 04/08/05 ***************************************
     O********         41 39N57          23 'Deposit Due *       '
     O********         42 39N57          23 'Credit *            '
     O********        N57 39 51XXDPDUBB  51
     O               41 39                       23 'Deposit Due *       '
     O               42 39                       23 'Credit *            '
     O                  39 51XXDPDU        BB    51
      ** END CHG ***** 04/08/05 ***************************************
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN CHG ***  04/12/99  ************************************
     O*****                    ELEXM22   58
     O*****                    XXEMD2    70 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************            ELEXM32   58
     O*************            XXEMD3    70 '  /  /    '
     O                       ELEXM3        2     61
     O                       XXEMD3              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  04/12/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTST1         1
     O               39 52                       23 'Membership Dues     '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 52 51XXMFEEBB  41
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 52 51XXMFEEBB  48
     O               39 52 51XXMFEE        BB    51
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*****                    ELEXM32   58
     O*****                    XXEMD3    70 '  /  /    '
     O*** END DEL *****  04/12/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTST1         1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 53             23 'Safety Fees Due     '
     O               39 53   XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 53 51XXSFEEBB  41
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39 53 51XXSFEEBB  48
     O               39 53 51XXSFEE        BB    51
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTST1         1
     O               39 55   XX1STH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 55 51XX1STBBB  41
     O*****           N46                63 'Deductible Plan:  '
     O*****           N46      XXDEDT    68
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 55 51XX1STP    32
     O************     39 55 51XX1STBBB  48
     O************    N46                68 'Deductible Plan:  '
     O************    N46      XXDEDT    73
     O               39 55 51X196          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              55          35 '%'
     O               39    55                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 55 51XX1STB        BB    51
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O          E            TOTST1         1
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTST1         1
     O               39 56   XX2NDH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39 56 51XX2NDBBB  41
     O*****           N46      XXDDD1    79
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     39 56 51XX2NDP    32
     O************     39 56 51XX2NDBBB  48
     O************    N46      XXDDD1    80
     O               39 56 51X296          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              56          35 '%'
     O               39    56                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 56 51XX2NDB        BB    51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          71 'Deductible Plan:  '
     O              N47      X2DEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            TOTST1         1
     O               39 64   XX3RDH              23
     O               39 64 51X396          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              64          35 '%'
     O               39    64                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 64 51XX3RDB        BB    51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47                          63 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            TOTST1         1
     O               39 91   XX4THH              23
     O               39 91 51X496          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              91          35 '%'
     O               39    91                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 91 51XX4THB        BB    51
     O              N46      DDAMPO        1     61 '$'
     O              N46N47                       76 'Per Occurrence'
     O              N46 47                       76 'Per Claim     '
     O          E            TOTST1         1
     O               39 98   XX5THH              23
     O               39 98 51X596          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              98          35 '%'
     O               39    98                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               39 98 51XX5THB        BB    51
     O              N46      DDAMPO        1     61 '$'
     O              N46N47                       76 'Per Occurrence'
     O              N46 47                       76 'Per Claim     '
     O*** END ADD *****  07/02/04  ************************************
     O          E            TOTST1         1
     O               39N35                       23 'Expense Constant Due'
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39N35 51XXECDUBB  41
     O*****           N46      DDAMPO1   56 '$'
     O*****           N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39N35 51XXECDUBB  48
     O               39N35 51XXECDU        BB    51
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN CHG ***  01/02/01  ************************************
     O*******         N46      DDAMPO1   61 '$'
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                76 'Per Occurrence'
     O*******         N46N47             76 'Per Occurrence'
     O*******         N46 47             76 'Per Claim     '
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  01/02/01  ************************************
      **** END   CHG **** 08/24/99 ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E    39    51TOTST1         0
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                              39 '___________'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************                      46 '___________'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************                       49 '___________'
     O              N57                          49 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTST1         1
     O*** BEGIN DEL ***  04/12/99  ************************************
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****            39N45             23 'State Tax Due       '
     O*******            N45 39XXTHD     23
     O*** END CHG *****  05/29/98  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39N45 51XXTXDUBB  41
     O*****           N46      DDAMAG1   56 '$'
     O*****           N46                73 'Deductible Limit'
     O*******          39N45 51XXTXDUBB  48
     O*** END DEL *****  04/12/99  ************************************
     O*** BEGIN DEL ***  01/02/01  ************************************
     O******          N46      DDAMAG1   61 '$'
     O******          N46                78 'Deductible Limit'
     O*** END DEL *****  01/02/01  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTST1         2
     O*** BEGIN CHG ***  11/15/99  ************************************
     O*********        39                23 'Total Due           '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            39    51XXTLDUBB  41 '$'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    39    51XXTLDUBB  48 '$'
     O*********        39    51XXTLDUBB  51 '$'
     O               39N57                       23 'Total Due           '
     O               39N57 51XXTLDU        BB    51 '$'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTST1         1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************           39          18 'Payment Plan:  '
     O*************          39XBDESC    43
     O                 N57 39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********          N57 39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********          N57 39XBDESC    48
     O                 N57 39XBDESC              43
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTST1         1
     O               43 72 39                    24 '* Payment Due No Later '
     O               43 72 39                    28 'Than'
     O               43 72 39XXDUED         B    39 '  /  /    '
     O                  73 39                    24 '* Please Deduct The Cre'
     O                  73 39                    43 'dit Indicated Above'
     O          E            TOTST1         1
     O                  73 39                    24 '  From Your First Month'
     O                  73 39                    43 'ly Payroll Report. '
     O                       BCCOM1              75
     O          E            TOTST1         1
     O                       BCCOM2              75
     O          E            TOTST1         1
     O                       BCCOM3              75
     O*
     O*  PRINT TOTALS FOR AUDITED ACCOUNTS
     O*
     O          E            TOTAT          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O************                               23 'Audited Premium     '
     O              N25                          23 'Audited Premium     '
     O               25                          23 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     41
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      66 'Experience Modifiers:'
     O              N95                          66 'Experience Modifiers:'
     O               95                          66 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O          E            TOTAT          1
     O               52                          23 'Membership Dues     '
     O               52    51XXMFEE        B     41
     O                       ELEXM1        2     53
     O                       XXEMD1              65 '  /  /    '
     O          E            TOTAT          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        53                23 'Safety Fees Due     '
     O               53      XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               53    51XXSFEE        BB    41
     O                       ELEXM2        2     53
     O                       XXEMD2              65 '  /  /    '
     O          E            TOTAT          1
     O                 N35                       23 'Expense Constant    '
     O                 N35 51XXEXCN        B     41
     O                       ELEXM3        2     53
     O                       XXEMD3              65 '  /  /    '
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTAT          1
     O               67                          23 'Installment Fees Due'
     O               67 51   XXINST        B     41
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  03/04/02  ************************************
     O          E            TOTAT          1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****              N45             23 'State Tax           '
     O                 N45   XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O              N45 51   ELTXDU        A     41
     O*** BEG ADD *****  08/01/05  ************************************
     O          E            TOTAT          1
     O                       XXGHDG              23
     O                     51XXGAMT        B     41
     O*** END ADD *****  08/01/05  ************************************
     O          E       51   TOTAT          0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************                       39 '___________'
     O              N57                          39 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTAT          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************                       23 'Previously Paid     '
     O************           51XXDPRLBB  41
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*********       N57                23 'Previously Paid     '
     O              N57   N92                    23 'Previously Paid     '
     O              N57    92                    23 'Previously Billed   '
     O*** END CHG *****  10/13/00  ************************************
     O              N57    51XXDPRL        BB    41
     O*** END CHG *****  11/15/99  ************************************
     O              N46                          63 'Deductible Plan:  '
     O              N46      XXDEDT              68
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O          E            TOTAT          1
     O              N46      XXAMPO        1     56 '$'
     O              N46      XXOCCL              72
     O***  END   ADD  ***  04/07/05 *************************************
     O          E            TOTAT          0
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************     41                23 'Balance Due *       '
     O************     42                23 'Credit *            '
     O************           51XXDPDUB   41 '$'
     O               41N57                       23 'Balance Due *       '
     O               42N57                       23 'Credit *            '
     O                 N57 51XXDPDU        B     41 '$'
     O*** END CHG *****  11/15/99  ************************************
     O          E            TOTAT          1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O************     41                23 'Balance Due *       '
     O************     42                23 'Credit *            '
     O************           51XXDPDUBB  41 '$'
     O               41N57                       23 'Balance Due *       '
     O               42N57                       23 'Credit *            '
     O                 N57 51XXDPDU        BB    41 '$'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    79
     O              N47                          63 'Deductible Plan:  '
     O              N47      X2DEDT              68
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O          E            TOTAT          1
     O*** BEGIN ADD *** 10/13/00  ************************************
     O              N57 37                       19 '<Agency Billing>'
     O*** END ADD ***** 10/13/00  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   56 '$'
     O***  END   CHG  ***  04/07/05 *************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                71 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             71 'Per Occurrence'
     O**********      N46 47             71 'Per Claim     '
     O              N47                          55 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O          E            TOTAT          1
     O*** BEGIN ADD ***  03/04/98  ************************************
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O**************** 41                24 '* Payment Due No Later '
     O**************** 41                28 'Than'
     O**************** 41      XXDUED    39 '  /  /    '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********      41N84             24 '* Payment Due No Later '
     O***********      41N84             28 'Than'
     O***********      41N84   XXDUED    39 '  /  /    '
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O*** END ADD *****  03/04/98  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   56 '$'
     O**********      N46                73 'Deductible Limit'
     O              N47      X2AMPO        1     56 '$'
     O              N47      X2OCCL              72
     O***  END   CHG  ***  04/07/05 *************************************
     O          E            TOTAT          1
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '* Please Deduct The Cre'
     O******************* 43             43 'dit Indicated Above'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O***********     N83 43             24 '* Please Deduct The Cre'
     O***********     N83 43             43 'dit Indicated Above'
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O              N47      X2AMAG        1     56 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          73 'Deductible Limit'
     O              N47                          66 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTAT          2
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '  From Your First Month'
     O******************* 43             43 'ly Payroll Report. '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********      N83 43             24 '  From Your First Month'
     O**********      N83 43             43 'ly Payroll Report. '
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTAT          1
     O                       BCCOM1              75
     O          E            TOTAT          1
     O                       BCCOM2              75
     O          E            TOTAT          1
     O                       BCCOM3              75
      *
     O****BEGIN ADD ***  04/17/06  ************************************
     O          E            TOTAT#         1
     O                                           36 'Billed'
     O                                           54 'Paid'
     O                                           78 'Balance Due'
     O          E            TOTAT#         1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O****************                           20 'Audited Premium  '
     O              N25                          20 'Audited Premium  '
     O               25                          20 'Audited Contr.   '
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     40
     O                     51XXPRPD        A     60
     O                     51XXPBAL        A     80
     O          E            TOTAT#         1
     O               52                          20 'Membership Dues  '
     O               52    51XXMFEE        B     40
     O               52    51XXMFPD        A     60
     O               52    51XXMBAL        A     80
     O          E            TOTAT#         1
     O               53                          20 'Safety Fees Due  '
     O               53    51XXSFEE        B     40
     O               53    51XXSFPD        A     60
     O               53    51XXSBAL        A     80
     O          E            TOTAT#         1
     O                 N35                       20 'Expense Constant '
     O                 N35 51XXEXCN        B     40
     O                 N35 51XXEXPD        A     60
     O                 N35 51XXEBAL        A     80
     O*** BEGIN ADD ***  06/05/08  *************************************
     O          E            TOTAT#         1
     O                       XXGHDG              23
     O                     51XXGAMT        A     40
     O                     51XXGPD         A     60
     O                     51XXFBAL        A     80
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTAT#         1
     O                 N28                       20 'NSF/Legal Fees   '
     O                 N28 51XXGFEE        B     40
     O                 N28 51XXGFPD        A     60
     O                 N28 51XXGBAL        A     80
     O          E            TOTAT#         0
     O              N57                          25 '________________________'
     O              N57                          48 '________________________'
     O              N57                          71 '________________________'
     O              N57                          80 '_________'
     O          E          51TOTAT#         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************      N45   XXTHD     20
     O                 N45   XXTHD               23
     O*** END CHG *****  06/05/08  *************************************
     O              N45 51   ELTXDU        A     40
     O              N45 51   XXTAXP        A     60
     O              N45 51   XXTBAL        A     80
     O          E            TOTAT#         2
     O              N57                          20 'Total            '
     O              N57      XXTBIL        A     40
     O              N57      XXDPRL        A     60
     O              N57      XXDPDU        A     80
     O*** BEGIN DEL ***  06/05/08  *************************************
     O******* E  1             TOTAT#
     O*******         N57 37             19 '<Agency Billing>'
     O*** END DEL *****  06/05/08  *************************************
     O          E            TOTAT#         1
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O*** BEGIN ADD ***  06/05/08  *************************************
     O              N57 37                       80 '<Agency Billing>'
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTAT#         1
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O          E            TOTAT#         2
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O          E            TOTAT#         1
     O                                           24 'Experience Modifiers:'
     O          E            TOTAT#         1
     O                       ELEXM1        2      9
     O                       XXEMD1              22 '  /  /    '
     O                       BCCOM1              75
     O          E            TOTAT#         1
     O                       ELEXM2        2      9
     O                       XXEMD2              22 '  /  /    '
     O                       BCCOM2              75
     O          E            TOTAT#         1
     O                       ELEXM3        2      9
     O                       XXEMD3              22 '  /  /    '
     O                       BCCOM3              75
     O          E            TOTAT#         1
     O              N46                          19 'Deductible Plan:'
     O              N46      XXDEDT              25
     O              N46      XXDDD1              53
     O          E            TOTAT#         1
     O              N46      DDAMPO        1     13 '$'
     O              N46N47                       28 'Per Occurrence'
     O              N46 47                       28 'Per Claim     '
     O          E            TOTAT#         1
     O              N46      DDAMAG        1     13 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          30 'Deductible Limit'
     O              N46                          23 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
      *** END ADD **** 04/17/06 **************************************
     O*
     O          E            TOTAT1         1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O****************                           23 'Audited Premium     '
     O              N25                          23 'Audited Premium     '
     O               25                          23 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  51XXPRO B   41
     O*****                              66 'Experience Modifiers:'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************          51XXPRO B   48
     O*************                      71 'Experience Modifiers:'
     O                     51XXPRO         B     51
     O*** BEGIN CHG ***  02/21/01  ************************************
     O*************                      74 'Experience Modifiers:'
     O              N95                          74 'Experience Modifiers:'
     O               95                          74 'Merit Rating:        '
     O*** END CHG *****  02/21/01  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O               52                          23 'Membership Dues     '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            52    51XXMFEEB   41
     O*****                    ELEXM12   53
     O*****                    XXEMD1    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     52    51XXMFEEB   48
     O************             ELEXM12   58
     O************             XXEMD1    70 '  /  /    '
     O               52    51XXMFEE        B     51
     O                       ELEXM1        2     61
     O                       XXEMD1              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        53                23 'Safety Fees Due     '
     O               53      XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            53    51XXSFEEB   41
     O*****                    ELEXM22   53
     O*****                    XXEMD2    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************    53    51XXSFEEB   48
     O*************            ELEXM22   58
     O*************            XXEMD2    70 '  /  /    '
     O               53    51XXSFEE        B     51
     O                       ELEXM2        2     61
     O                       XXEMD2              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O               55      XX1STH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            55    51XX1STBB   41
     O*****                    ELEXM32   53
     O*****                    XXEMD3    65 '  /  /    '
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************     55    51XX1STP    32
     O************     55    51XX1STBB   48
     O************             ELEXM32   58
     O************             XXEMD3    70 '  /  /    '
     O               55    51X196          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              55          35 '%'
     O               39    55                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               55    51XX1STB        B     51
     O                       ELEXM3        2     61
     O                       XXEMD3              73 '  /  /    '
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O               56      XX2NDH              23
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****            56    51XX2NDBB   41
     O*** BEGIN CHG ***  10/19/99  ************************************
     O***********      56    51XX2NDP    32
     O***********      56    51XX2NDBB   48
     O               56    51X296          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              56          35 '%'
     O               39    56                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               56    51XX2NDB        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46                          71 'Deductible Plan:  '
     O              N46      XXDEDT              76
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            TOTAT1         1
     O               64      XX3RDH              23
     O               64    51X396          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              64          35 '%'
     O               39    64                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               64    51XX3RDB        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMPO        1     61 '$'
     O              N46      XXOCCL              77
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            TOTAT1         1
     O               91      XX4THH              23
     O               91    51X496          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              91          35 '%'
     O               39    91                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               91    51XX4THB        B     51
     O          E            TOTAT1         1
     O               98      XX5THH              23
     O               98    51X596          4     34
     O*** BEGIN CHG ***  11/29/04  ************************************
     O*********              98          35 '%'
     O               39    98                    35 '%'
     O*** END CHG *****  11/29/04  ************************************
     O               98    51XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            TOTAT1         1
     O               67                          23 'Installment Fees Due'
     O               67 51   XXINST        B     51
     O***  BEGIN ADD  ***  04/07/05 *************************************
     O              N46      XXAMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          78 'Deductible Limit'
     O              N46                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   ADD  ***  04/07/05 *************************************
     O*** END ADD *****  03/04/02  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O                 N35                       23 'Expense Constant    '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****              N35 51XXEXCNB   41
     O*****           N46                63 'Deductible Plan:  '
     O*****           N46      XXDEDT    68
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************       N35 51XXEXCNB   48
     O************    N46                68 'Deductible Plan:  '
     O************    N46      XXDEDT    73
     O                 N35 51XXEXCN        B     51
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46                71 'Deductible Plan:  '
     O**********      N46      XXDEDT    76
     O              N47                          71 'Deductible Plan:  '
     O              N47      X2DEDT              76
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O*** BEGIN CHG ***  05/29/98  ************************************
     O*****           N45                23 'State Tax           '
     O                 N45   XXTHD               23
     O*** END CHG *****  05/29/98  ************************************
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****           N45 51   ELTXDUA   41
     O*****           N46      XXDDD1    79
     O*** BEGIN CHG ***  10/19/99  ************************************
     O************    N45 51   ELTXDUA   48
     O              N45 51   ELTXDU        A     51
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      XXDDD1    80
     O              N47                          63 'Effective:'
     O              N47      X82DEF            +  1 '  /  /    '
     O***  END   CHG  ***  04/07/05 *************************************
     O*** BEG ADD *****  08/01/05  ************************************
     O          E            TOTAT1         1
     O                       XXGHDG              23
     O                     51XXGAMT        B     51
     O*** END ADD *****  08/01/05  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E          51TOTAT1         0
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                              39 '___________'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************                      46 '___________'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********                         49 '___________'
     O              N57                          49 '___________'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O*** END CHG *****  07/17/98  ************************************
     O          E            TOTAT1         1
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********                         23 'Previously Paid     '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  51XXDPRLB   41
     O*****           N46      DDAMPO1   56 '$'
     O*****           N46                71 'Per Occurrence'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************          51XXDPRLB   48
     O**********             51XXDPRLB   51
     O*** BEGIN CHG ***  10/13/00  ************************************
     O*********       N57                23 'Previously Paid     '
     O              N57   N92                    23 'Previously Paid     '
     O              N57    92                    23 'Previously Billed   '
     O*** END CHG *****  10/13/00  ************************************
     O              N57    51XXDPRL        B     51
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMPO1   61 '$'
     O***  END   CHG  ***  04/07/05 *************************************
      **** BEGIN CHG **** 08/24/99 ************************************
     O*************** N46                76 'Per Occurrence'
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46N47             76 'Per Occurrence'
     O**********      N46 47             76 'Per Claim     '
     O              N47      X2AMPO        1     61 '$'
     O              N47      X2OCCL              77
     O***  END   CHG  ***  04/07/05 *************************************
      **** END   CHG **** 08/24/99 ************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN CHG *** 10/13/00  ************************************
     O*****   E  2             TOTAT1
     O          E            TOTAT1         1
     O*** END CHG ***** 10/13/00  ************************************
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********       41                23 'Balance Due *       '
     O**********       42                23 'Credit *            '
     O*** BEGIN CHG ***  07/17/98  ************************************
     O*****                  51XXDPDUB   41 '$'
     O*****           N46      DDAMAG1   56 '$'
     O*****           N46                73 'Deductible Limit'
     O*** BEGIN CHG ***  10/19/99  ************************************
     O*************          51XXDPDUB   48 '$'
     O**********             51XXDPDUB   51 '$'
     O               41N57                       23 'Balance Due *       '
     O               42N57                       23 'Credit *            '
     O                 N57 51XXDPDU        B     51 '$'
     O*** END CHG *****  11/15/99  ************************************
     O*** END CHG *****  10/19/99  ************************************
     O***  BEGIN CHG  ***  04/07/05 *************************************
     O**********      N46      DDAMAG1   61 '$'
     O**********      N46                78 'Deductible Limit'
     O              N47      X2AMAG        1     61 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N47                          78 'Deductible Limit'
     O              N47                          71 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
     O***  END   CHG  ***  04/07/05 *************************************
     O*** END CHG *****  07/17/98  ************************************
     O*** BEGIN ADD *** 10/13/00  ************************************
     O          E            TOTAT1         1
     O              N57 37                       19 '<Agency Billing>'
     O*** END ADD ***** 10/13/00  ************************************
     O          E            TOTAT1         1
     O*** BEGIN ADD ***  03/04/98  ************************************
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O**************** 41                24 '* Payment Due No Later '
     O**************** 41                28 'Than'
     O**************** 41      XXDUED    39 '  /  /    '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********       41N84             24 '* Payment Due No Later '
     O**********       41N84             28 'Than'
     O**********       41N84   XXDUED    39 '  /  /    '
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O*** END ADD *****  03/04/98  ************************************
     O          E            TOTAT1         1
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '* Please Deduct The Cre'
     O******************* 43             43 'dit Indicated Above'
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********      N83 43             24 '* Please Deduct The Cre'
     O**********      N83 43             43 'dit Indicated Above'
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTAT1         1
     O***  BEGIN CHG  ***  03/04/99  **********************************
     O******************* 43             24 '  From Your First Month'
     O******************* 43             43 'ly Payroll Report. '
     O*** BEGIN CHG ***  11/15/99  ************************************
     O**********      N83 43             24 '  From Your First Month'
     O**********      N83 43             43 'ly Payroll Report. '
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O*** END CHG *****  11/15/99  ************************************
     O***  END   CHG  ***  03/04/99  **********************************
     O          E            TOTAT1         1
     O                       BCCOM1              75
     O          E            TOTAT1         1
     O                       BCCOM2              75
     O          E            TOTAT1         1
     O                       BCCOM3              75
     O****BEGIN ADD ***  04/17/06  ************************************
     O          E            TOTA#1         1
     O                                           36 'Billed'
     O                                           54 'Paid'
     O                                           78 'Balance Due'
     O          E            TOTA#1         1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O************                               20 'Audited Premium  '
     O              N25                          20 'Audited Premium  '
     O               25                          20 'Audited Contr.   '
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     40
     O                     51XXPRPD        A     60
     O                     51XXPBAL        A     80
     O          E            TOTA#1         1
     O               52                          20 'Membership Dues  '
     O               52    51XXMFEE        B     40
     O               52    51XXMFPD        A     60
     O               52    51XXMBAL        A     80
     O          E            TOTA#1         1
     O               53                          20 'Safety Fees Due  '
     O               53    51XXSFEE        B     40
     O               53    51XXSFPD        A     60
     O               53    51XXSBAL        A     80
     O          E            TOTA#1         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************    55      XX1STH    20
     O               55      XX1STH              23
     O*** END CHG *****  06/05/08  *************************************
     O*****MLD         55    51X196  4   30
     O*****MLD         55                31 '%'
     O               55    51XX1STB        B     40
     O               55    51XX1SPD        A     60
     O               55    51XX1BAL        A     80
     O          E            TOTA#1         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************    56      XX2NDH    20
     O               56      XX2NDH              23
     O*** END CHG *****  06/05/08  *************************************
     O*****MLD         56    51X296  4   30
     O*****MLD         56                31 '%'
     O               56    51XX2NDB        B     40
     O               56    51XX2NPD        A     60
     O               56    51XX2BAL        A     80
     O          E            TOTA#1         1
     O                 N35                       20 'Expense Constant '
     O                 N35 51XXEXCN        B     40
     O                 N35 51XXEXPD        A     60
     O                 N35 51XXEBAL        A     80
     O*** BEGIN ADD ***  06/05/08  *************************************
     O          E            TOTA#1         1
     O                       XXGHDG              23
     O                     51XXGAMT        A     40
     O                     51XXGPD         A     60
     O                     51XXFBAL        A     80
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTA#1         1
     O                 N28                       20 'NSF/Legal Fees   '
     O                 N28 51XXGFEE        B     40
     O                 N28 51XXGFPD        A     60
     O                 N28 51XXGBAL        A     80
     O          E            TOTA#1         0
     O              N57                          25 '________________________'
     O              N57                          48 '________________________'
     O              N57                          71 '________________________'
     O              N57                          80 '_________'
     O          E          51TOTA#1         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************      N45   XXTHD     20
     O                 N45   XXTHD               23
     O*** END CHG *****  06/05/08  *************************************
     O              N45 51   ELTXDU        A     40
     O              N45 51   XXTAXP        A     60
     O              N45 51   XXTBAL        A     80
     O          E            TOTA#1         2
     O              N57                          20 'Total            '
     O              N57      XXTBIL        A     40
     O              N57      XXDPRL        A     60
     O              N57      XXDPDU        A     80
     O*** BEGIN DEL ***  06/05/08  *************************************
     O******* E  1             TOTA#1
     O*******         N57 37             19 '<Agency Billing>'
     O*** END DEL *****  06/05/08  *************************************
     O          E            TOTA#1         1
     O               41N84N57                    24 '* Payment Due No Later '
     O               41N84N57                    28 'Than'
     O               41N84N57XXDUED              39 '  /  /    '
     O*** BEGIN ADD ***  06/05/08  *************************************
     O              N57 37                       80 '<Agency Billing>'
     O*** END ADD *****  06/05/08  *************************************
     O          E            TOTA#1         1
     O              N83 43N57                    24 '* Please Deduct The Cre'
     O              N83 43N57                    43 'dit Indicated Above'
     O          E            TOTA#1         1
     O              N83 43N57                    24 '  From Your First Month'
     O              N83 43N57                    43 'ly Payroll Report. '
     O          E            TOTA#1         1
     O                                           24 'Experience Modifiers:'
     O          E            TOTA#1         1
     O                       ELEXM1        2      9
     O                       XXEMD1              22 '  /  /    '
     O                       BCCOM1              75
     O          E            TOTA#1         1
     O                       ELEXM2        2      9
     O                       XXEMD2              22 '  /  /    '
     O                       BCCOM2              75
     O          E            TOTA#1         1
     O                       ELEXM3        2      9
     O                       XXEMD3              22 '  /  /    '
     O                       BCCOM3              75
     O          E            TOTA#1         1
     O              N46                          19 'Deductible Plan:'
     O              N46      XXDEDT              25
     O              N46      XXDDD1              53
     O          E            TOTA#1         1
     O              N46      DDAMPO        1     13 '$'
     O              N46N47                       28 'Per Occurrence'
     O              N46 47                       28 'Per Claim     '
     O          E            TOTA#1         1
     O              N46      DDAMAG        1     13 '$'
     O*** Begin Chg ***  06/06/11  ************************************
     O**********    N46                          30 'Deductible Limit'
     O              N46                          23 'Aggregate'
     O*** End Chg *****  06/06/11  ************************************
      *** END CHG ***** 04/17/06 **************************************
     O*
     O*  PRINT TOTALS THAT ARE COMMON TO BOTH ACCOUNTS.
     O*
     O          E            TOTFT          1 64
     O***** beg chg ******* 11/22/13 ******************************************
     O*****                                      37 'Make checks payable to '
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    FNSHNM    67
     O*****                  XXCKNM              67
     O*** END CHG *****  06/01/98  ************************************
     O               28                          37 'Make checks payable to '
     O               28      xxCkNm              67
     O***** end chg ******* 11/22/13 ******************************************
     O                                           76 'Page'
     O                       XXPAG4        Z     80
     O*++ BEGIN ADD +++  09/13/06  ++++++++++++++++++++++++++++++++++++
     O*
     O* PRINT NOTICE OF ELECTION OF RETROSPECTIVE RATING PLAN.
     O* (RESOLVED)
     O*
     OQSYSPRT   E            NOTE           1 04
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            NOTE           1
     O                                           50 'NOTICE OF ELECTION'
     O          E            NOTE           1
     O                                           42 'OF'
     O          E            NOTE           2
     O                                           49 'RETROSPECTIVE RATING'
     O                                           54 'PLAN'
     O          E            NOTE           1
     O                                           26 'The undersigned certifie'
     O                                           50 's that the named insured'
     O                                           74 ' has elected the use of '
     O                                           77 'the'
     O          E            NOTE           1
     O                                           26 'Retrospective Rating Pla'
     O                                           50 'n as detailed below. It '
     O                                           74 'is also certified that t'
     O                                           76 'he'
     O          E            NOTE           1
     O                                           26 'insured understands all '
     O                                           50 'terms, conditions and pr'
     O                                           74 'ovisions of the Plan, in'
     O                                           79 'clud-'
     O          E            NOTE           2
     O                                           26 'ing the method of premiu'
     O                                           50 'm computation, payment, '
     O                                           74 'and penalties for cancel'
     O                                           80 'ation.'
     O          E            NOTE           2
     O                                           26 'The Plan shall apply to '
     O                                           50 'the policy indicated bel'
     O                                           53 'ow:'
     O          E            NOTE           0
     O                       XXNAME              64
     O          E            NOTE           2
     O                                           26 '1.  Name of Insured ____'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTE           0
     O                       XXAD1               67
     O          E            NOTE           2
     O                                           26 '2.  Address of Insured _'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTE           0
     O                       XXAD2               67
     O          E            NOTE           2
     O                                           26 '                       _'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTE           0
     O                       XXAD3               67
     O          E            NOTE           2
     O                                           26 '                       _'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTE           2
     O                                           26 '3.  Policy Number       '
     O                                           74 'Effective Date          '
     O          E            NOTE           0
     O                       QQPOL#              23
     O                       XXFROM        Y     62
     O          E            NOTE           2
     O                                           30 '________________________'
     O                                           36 '______'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTE           2
     O                                           26 '4.  Rating Variables:   '
     O          E            NOTE           0
     O                       ELMNFC        2     42
     O          E            NOTE           2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O**************                             30 'A.  Minimum Premium Fact'
     O              N25                          30 'A.  Minimum Premium Fact'
     O               25                          30 'A.  Minimum Contr. Fact'
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 'or _________________'
     O          E            NOTE           0
     O                       ELMXFC        2     42
     O          E            NOTE           2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*************                              30 'B.  Maximum Premium Fact'
     O              N25                          30 'B.  Maximum Premium Fact'
     O               25                          30 'B.  Maximum Contr. Fact'
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 'or _________________'
     O          E            NOTE           0
     O                       ELLCVF        2     42
     O          E            NOTE           2
     O                                           30 'C.  Loss Conversion Fact'
     O                                           50 'or _________________'
     O          E            NOTE           0
     O                       ELBSFC        2     42
     O          E            NOTE           2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*************                              30 'D.  Basic Premium Factor'
     O              N25                          30 'D.  Basic Premium Factor'
     O               25                          30 'D.  Basic Contr. Factor'
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 ' ___________________'
     O          E            NOTE           0
     O                       ELTAX         2     42
     O                       ELTAXF        2     70
     O          E            NOTE           2
     O                                           30 'E.  Tax Multiplier _____'
     O                                           50 '____________________'
     O                                           76 'F-Classes ______________'
     O                                           80 '____'
     O          E            NOTE           2
     O                                           30 'F.  Term of Plan: 1 Year'
     O          E            NOTE           2
     O                                           26 '5.  Elective Rating Elem'
     O                                           31 'ents:'
     O          E            NOTE           0
     O                       ELELP         2     45
     O                       ELELPF        2     70
     O          E            NOTE           2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O***************                            30 'A.  Excess Loss Premium '
     O              N25                          30 'A.  Excess Loss Premium '
     O               25                          30 'A.  Excess Loss Contr.  '
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 'Factor _____________'
     O                                           76 'F-Classes ______________'
     O                                           80 '____'
     O          E            NOTE           0
     O                       ELRDF1        2     55
     O                       ELRDF2        2     67
     O                       ELRDF3        2     79
     O          E            NOTE           2
     O                                           30 'B.  Retrospective Develo'
     O                                           54 'pment Factors: #1 ______'
     O                                           78 '__ #2 ________ #3 ______'
     O                                           80 '__'
     O          E            NOTE           0
     O                       ELRETN        2     56
     O          E            NOTE           2
     O                                           26 '6.  Loss Limitation (if '
     O                                           50 'applicable) ____________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTE           1 60
     O                                           30 '________________________'
     O                                           42 '____________'
     O                                           80 '________________________'
     O          E            NOTE           1
     O                                           34 'Signature of Insured'
     O                                           80 '      Date Signed       '
     O          E            NOTE           2
     O                                           30 '(Proprietor, Partner, or'
     O                                           54 ' Authorized Officer of C'
     O                                           78 'orporation)             '
     O          E            NOTE           1
     O                                           28 'Note: This letter is sub'
     O                                           52 'ject to the Retrospectiv'
     O                                           76 'e Rating Plan(s) in effe'
     O                                           79 'ct.'
     O*
     O* PRINT NOTICE OF ELECTION OF RETROSPECTIVE RATING PLAN.
     O* (HARDCOPY)
     O*
     OQSYSPRT1  E            NOTET          1 04
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            NOTET          1
     O                                           50 'NOTICE OF ELECTION'
     O          E            NOTET          1
     O                                           42 'OF'
     O          E            NOTET          2
     O                                           49 'RETROSPECTIVE RATING'
     O                                           54 'PLAN'
     O          E            NOTET          1
     O                                           26 'The undersigned certifie'
     O                                           50 's that the named insured'
     O                                           74 ' has elected the use of '
     O                                           77 'the'
     O          E            NOTET          1
     O                                           26 'Retrospective Rating Pla'
     O                                           50 'n as detailed below. It '
     O                                           74 'is also certified that t'
     O                                           76 'he'
     O          E            NOTET          1
     O                                           26 'insured understands all '
     O                                           50 'terms, conditions and pr'
     O                                           74 'ovisions of the Plan, in'
     O                                           79 'clud-'
     O          E            NOTET          2
     O                                           26 'ing the method of premiu'
     O                                           50 'm computation, payment, '
     O                                           74 'and penalties for cancel'
     O                                           80 'ation.'
     O          E            NOTET          2
     O                                           26 'The Plan shall apply to '
     O                                           50 'the policy indicated bel'
     O                                           53 'ow:'
     O          E            NOTET          0
     O                       XXNAME              64
     O          E            NOTET          2
     O                                           26 '1.  Name of Insured ____'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTET          0
     O                       XXAD1               67
     O          E            NOTET          2
     O                                           26 '2.  Address of Insured _'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTET          0
     O                       XXAD2               67
     O          E            NOTET          2
     O                                           26 '                       _'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTET          0
     O                       XXAD3               67
     O          E            NOTET          2
     O                                           26 '                       _'
     O                                           50 '________________________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTET          2
     O                                           26 '3.  Policy Number       '
     O                                           74 'Effective Date          '
     O          E            NOTET          0
     O                       QQPOL#              23
     O                       XXFROM        Y     62
     O          E            NOTET          2
     O                                           30 '________________________'
     O                                           36 '______'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTET          2
     O                                           26 '4.  Rating Variables:   '
     O          E            NOTET          0
     O                       ELMNFC        2     42
     O          E            NOTET          2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O******************                         30 'A.  Minimum Premium Fact'
     O              N25                          30 'A.  Minimum Premium Fact'
     O               25                          30 'A.  Minimum Contr. Fact'
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 'or _________________'
     O          E            NOTET          0
     O                       ELMXFC        2     42
     O          E            NOTET          2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*************                              30 'B.  Maximum Premium Fact'
     O              N25                          30 'B.  Maximum Premium Fact'
     O               25                          30 'B.  Maximum Contr. Fact'
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 'or _________________'
     O          E            NOTET          0
     O                       ELLCVF        2     42
     O          E            NOTET          2
     O                                           30 'C.  Loss Conversion Fact'
     O                                           50 'or _________________'
     O          E            NOTET          0
     O                       ELBSFC        2     42
     O          E            NOTET          2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*****************                          30 'D.  Basic Premium Factor'
     O              N25                          30 'D.  Basic Premium Factor'
     O               25                          30 'D.  Basic Contr. Factor'
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 ' ___________________'
     O          E            NOTET          0
     O                       ELTAX         2     42
     O                       ELTAXF        2     70
     O          E            NOTET          2
     O                                           30 'E.  Tax Multiplier _____'
     O                                           50 '____________________'
     O                                           76 'F-Classes ______________'
     O                                           80 '____'
     O          E            NOTET          2
     O                                           30 'F.  Term of Plan: 1 Year'
     O          E            NOTET          2
     O                                           26 '5.  Elective Rating Elem'
     O                                           31 'ents:'
     O          E            NOTET          0
     O                       ELELP         2     45
     O                       ELELPF        2     70
     O          E            NOTET          2
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*****************                          30 'A.  Excess Loss Premium '
     O                                           30 'A.  Excess Loss Contr. '
     O                                           30 'A.  Excess Loss Contr. '
     O*** END   CHG ****** 01/14/10 ************************************
     O                                           50 'Factor _____________'
     O                                           76 'F-Classes ______________'
     O                                           80 '____'
     O          E            NOTET          0
     O                       ELRDF1        2     55
     O                       ELRDF2        2     67
     O                       ELRDF3        2     79
     O          E            NOTET          2
     O                                           30 'B.  Retrospective Develo'
     O                                           54 'pment Factors: #1 ______'
     O                                           78 '__ #2 ________ #3 ______'
     O                                           80 '__'
     O          E            NOTET          0
     O                       ELRETN        2     56
     O          E            NOTET          2
     O                                           26 '6.  Loss Limitation (if '
     O                                           50 'applicable) ____________'
     O                                           74 '________________________'
     O                                           80 '______'
     O          E            NOTET          1 60
     O                                           30 '________________________'
     O                                           42 '____________'
     O                                           80 '________________________'
     O          E            NOTET          1
     O                                           34 'Signature of Insured'
     O                                           80 '      Date Signed       '
     O          E            NOTET          2
     O                                           30 '(Proprietor, Partner, or'
     O                                           54 ' Authorized Officer of C'
     O                                           78 'orporation)             '
     O          E            NOTET          1
     O                                           28 'Note: This letter is sub'
     O                                           52 'ject to the Retrospectiv'
     O                                           76 'e Rating Plan(s) in effe'
     O                                           79 'ct.'
     O*++ END ADD +++++  09/13/06  ++++++++++++++++++++++++++++++++++++
     O*** BEGIN ADD ***  11/15/99  ************************************
     OQSYSPRT1  E            SUMHT          1 02
     O               63N38   TIT(1)              56
     O              N63N38   TIT(2)              56
     O*** BEGIN ADD ***  10/21/03  ************************************
     O*********           38   TIT,3     56
     O              N30 38   TIT(3)              56
     O               30 38   TIT(4)              56
     O*** END ADD *****  10/21/03  ************************************
     O          E            SUMHT          1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMHT          1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            SUMHT          1
     O                       FNMAD1              42
     O**** BEGIN CHG **** 07/02/01 ************************************
     O****************** N38             58 'Policy #:     '
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O**** END   CHG **** 07/02/01 ************************************
     O                  38                       58 'Quote #:      '
     O                       QQPOL#              80
     O          E            SUMHT          1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            SUMHT          1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            SUMHT          1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            SUMHT          1
     O                       EMN(1)              39
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            58 'Agent #:      '
     O*******                  AHAGT#Z   64
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*** END CHG *** 09/24/04 ************************************
     O          E            SUMHT          1
     O                       EMN(2)              39
     O                       ANAME               80
     O          E            SUMHT          1
     O                       EMN(3)              39
     O                       AADD1               80
     O          E            SUMHT          1
     O                       EMN(4)              39
     O                       ACSZ                80
     O          E            SUMHT          1
     O                       EMN(5)              39
     O               69                          47 '('
     O               69      AGARE1              50
     O               69                          51 ')'
     O               69      AGPHN1              60 '   -    '
     O          E            SUMHT          1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMH2T         1
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********                           7 'Div'
     O*********                          10 'St'
     O*********                          16 'Code'
     O                                            6 'Div'
     O                                            9 'St'
     O                                           15 'Code'
     O*** END CHG *****  11/14/02  ************************************
     O                                           28 'Eff Date'
      **** BEGIN CHG **** 11/21/01 ************************************
     O********************************** 43 'Classification'
     O********************************** 62 'Payroll'
      **** beg chg ***** 01/24/11 *************************************
     O***********                                38 'Classification'
     O                                           43 'Classification'
      **** end chg ***** 01/24/11 *************************************
     O                                           57 'Payroll'
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********                         69 'Rate'
     O********************************** 71 'Rate'
     O                                           66 'Rate'
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O***************                            80 'Premium'
     O              N25                          80 'Premium'
     O               25                          80 'Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O********************************** 85 'Premium'
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 11/21/01 ************************************
     O          E            SUMH2T         2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
      **** BEGIN CHG **** 11/21/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O                                           80 '_________'
     O********************************** 85 '______________'
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 11/21/01 ************************************
     O          E            SUMDT          1
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********                EPDIV Z    7
     O*********                XXST      10
     O*********                EPCLASZ   16
     O*********                EPSPEC    17
     O                       EPDIV         Z      6
     O                       XXST                 9
     O                       EPCLAS        Z     15
     O                       EPSPEC              17
     O*** END CHG *****  11/14/02  ************************************
     O*** BEG ADD *****  04/20/09  ************************************
     O*** beg del ***** 01/24/11 **************************************
     O**********             EPEXT               23
     O*** end del ***** 01/24/11 **************************************
     O*** END ADD *****  04/20/09  ************************************
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            XPEFDT              28 '  /  /    '
     O*** BEG CHG ***** 01/24/11 **************************************
     O***********            XPEFDT              33 '  /  /    '
     O                       XPEFDT              28 '  /  /    '
     O*** BEG CHG ***** 01/24/11 **************************************
     O*** END CHG *****  04/20/09  ************************************
      **** BEGIN CHG **** 11/21/01 ************************************
     O************************ XXDESC    49
     O************************ XXEAPR2   62
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            XXDESC              44
     O***********            XXEAPR        2     57
     O*** BEG CHG ***** 01/24/11 **************************************
     O***********            XXDESC              49
     O***********            XXEAPR        2     62
     O                       XXDESC              44
     O                       XXEAPR        2     57
     O*** end CHG ***** 01/24/11 **************************************
     O*** END CHG *****  04/20/09  ************************************
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********               EPARAT2   69
     O************************ EPARAT2   71
     O                       EPARAT        2     66
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O                       XXEMPR        1     80
     O*** BEGIN ADD ****** 10/13/11 ************************************
     O                       XXEMSN              81
     O*** END   ADD ****** 10/13/11 ************************************
     O************************ XXEMPR1   85
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 11/21/01 ************************************
     O*
     O          E            SUMET1         0 40
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMET          2
     O          E            SUMET          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O************   63    39                    25 'Premium               '
     O************  N63    39                    25 'Revised Premium       '
     O               63N25 39                    25 'Premium               '
     O              N63N25 39                    25 'Revised Premium       '
     O               63 25 39                    25 'Contribution          '
     O              N63 25 39                    25 'Revised Contribution  '
     O*** END   CHG ****** 01/14/10 ************************************
     O                  51 39XXPRO         AB    51
     O          E            SUMET          1
     O               39 51 52                    25 'Membership Dues       '
     O               39 51 52XXMFDU        AB    51
     O          E            SUMET          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 51 53          25 'Safety Fees Due       '
     O               39 51 53XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 51 53XXSFDU        BB    51
     O          E            SUMET          1
     O               39 51 55XX1STH              23
     O               39 51 55XX1STB        BB    51
     O          E            SUMET          1
     O               39 51 56XX2NDH              23
     O               39 51 56XX2NDB        BB    51
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            SUMET          1
     O               39 51 64XX3RDH              23
     O               39 51 64XX3RDB        BB    51
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            SUMET          1
     O               39 51 91XX4THH              23
     O               39 51 91XX4THB        BB    51
     O          E            SUMET          1
     O               39 51 98XX5THH              23
     O               39 51 98XX5THB        BB    51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            SUMET          1
     O               67    39                    25 'Installment Fees Due  '
     O               67 51 39XXINST        BB    51
     O*** END ADD *****  03/04/02  ************************************
     O          E            SUMET          1
     O              N44    39                    25 'Expense Constant      '
     O              N44 51 39XXEXCN        AB    51
     O          E            SUMET          1
     O                 N45 39XXTHD               23
     O              N45 51 39XXTXDU        BB    51
     O          E       51   SUMET          0
     O                     39                    49 '___________'
     O          E            SUMET          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O************** 63    39                    25 'Premium Paid          '
     O               63N25 39                    25 'Premium Paid          '
     O               63 25 39                    25 'Contribution Paid     '
     O*** END   CHG ****** 01/14/10 ************************************
     O              N63    39                    25 'Previously Paid       '
     O                  51 39XXDPRP        AB    51
     O          E            SUMET          2
     O                     39                    25 'Balance               '
     O                  51 39XXBAL         AB    51 '$'
     O          E            SUMET          1
     O                     39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********              39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********              39XBDESC    48
     O                     39XBDESC              43
     O               78 39   DUW(8)              48
     O                  39   DDU(8)              67
     O               78 39 51AMT(8)        BB    80
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O          E            SUMET          1
     O               71 39   DUW(1)               6
     O               63 39   DDU(1)              25
     O              N63 39 71                    11 'thru'
     O              N63 39   DDU(1)              30
     O               71 39 51AMT(1)        BB    44
      *** BEG CHG **** 06/14/02 ****************************************
     O***************  81 39   DUW,11    48
     O***************     39   DDU,11    67
     O***************  81 39 51AMT,11BB  80
     O*** BEGIN CHG ***  07/09/02  ************************************
     O********         81 39   DUW,10    48
     O********            39   DDU,10    67
     O********         81 39 51AMT,10BB  80
      *** BEG CHG **** 07/02/04 ****************************************
     O***************  80 39   DUW,10    48
     O***************     39   DDU,10    67
     O***************  80 39 51AMT,10BB  80
     O               79 39   DUW(9)              48
     O                  39   DDU(9)              67
     O               79 39 51AMT(9)        BB    80
      *** END CHG **** 07/02/04 ****************************************
     O*** END CHG *****  07/09/02  ************************************
      *** END CHG **** 06/14/02 ****************************************
     O          E            SUMET          1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        72 39   DUW,2      6
     O*********           39   DDU,2     25
     O               72 39N86DUW(2)               6
     O                  39N86DDU(2)              25
     O                  39 86ZZ            2      5
     O*** BEG CHG ***** 07/17/08 **************************************
     O***********         39 86          25 'Weekly Installments'
     O*** BEGIN ADD ***  04/25/03  ************************************
     O***********      36    86          30 'Installments on 1st/15th'
     O*** END ADD *****  04/25/03  ************************************
     O                  39 86INSTVB              30
     O*** END CHG ***** 07/17/08 **************************************
     O*** END CHG *****  04/21/03  ************************************
     O               72 39 51AMT(2)        BB    44
      *** BEG CHG **** 06/14/02 ****************************************
     O***************  82 39   DUW,12    48
     O***************     39   DDU,12    67
     O***************  82 39 51AMT,12BB  80
      *** BEG CHG **** 07/02/04 ****************************************
     O***************  81 39   DUW,11    48
     O***************     39   DDU,11    67
     O***************  81 39 51AMT,11BB  80
     O               80 39   DUW(10)             48
     O                  39   DDU(10)             67
     O               80 39 51AMT(10)       BB    80
      *** END CHG **** 07/02/04 ****************************************
      *** END CHG **** 06/14/02 ****************************************
     O          E            SUMET          1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        73 39   DUW,3      6
     O*********           39   DDU,3     25
     O               73 39N86DUW(3)               6
     O                  39N86DDU(3)              25
     O                  39 86                    11 'Starting'
     O                  39 86X1WKDT              22 '  /  /    '
     O                  39 86                    25 'of'
     O*** END CHG *****  04/21/03  ************************************
     O               73 39 51AMT(3)        BB    44
      *** BEG ADD **** 06/14/02 ****************************************
      *** BEG CHG **** 07/02/04 ****************************************
     O***********      82 39   DUW,12    48
     O***********         39   DDU,12    67
     O***********      82 39 51AMT,12BB  80
     O               81 39   DUW(11)             48
     O                  39   DDU(11)             67
     O               81 39 51AMT(11)       BB    80
      *** END CHG **** 07/02/04 ****************************************
      *** END ADD **** 06/14/02 ****************************************
     O          E            SUMET          1
     O               74 39   DUW(4)               6
     O                  39   DDU(4)              25
     O               74 39 51AMT(4)        BB    44
      *** BEG DEL **** 07/02/04 ****************************************
     O**********************   BCCOM1    75
      *** END DEL **** 07/02/04 ****************************************
      *** BEG ADD **** 07/02/04 ****************************************
     O               82 39   DUW(12)             48
     O                  39   DDU(12)             67
     O               82 39 51AMT(12)       BB    80
      *** END ADD **** 07/02/04 ****************************************
     O          E            SUMET          1
     O               75 39   DUW(5)               6
     O                  39   DDU(5)              25
     O               75 39 51AMT(5)        BB    44
      *** BEG CHG **** 07/02/04 ****************************************
     O**********************   BCCOM2    75
     O                       BCCOM1              75
      *** BEG CHG **** 07/02/04 ****************************************
     O          E            SUMET          1
     O               76 39   DUW(6)               6
     O                  39   DDU(6)              25
     O               76 39 51AMT(6)        BB    44
      *** BEG CHG **** 07/02/04 ****************************************
     O**********************   BCCOM3    75
     O                       BCCOM2              75
      *** BEG CHG **** 07/02/04 ****************************************
     O          E            SUMET          1
     O               77 39   DUW(7)               6
     O                  39   DDU(7)              25
     O               77 39 51AMT(7)        BB    44
      *** BEG ADD **** 07/02/04 ****************************************
     O                       BCCOM3              75
      *** END ADD **** 07/02/04 ****************************************
      *** BEG DEL **** 07/02/04 ****************************************
     O********E**1             SUMET
     O***********      78 39   DUW,8      6
     O***********         39   DDU,8     25
     O***********      78 39 51AMT,8 BB  44
     O********E**1             SUMET
     O***********      79 39   DUW,9      6
     O***********         39   DDU,9     25
     O***********      79 39 51AMT,9 BB  44
      *** END DEL **** 07/02/04 ****************************************
      *** BEG DEL **** 06/14/02 ****************************************
     O********E**1***          SUMET
     O***************  80 39   DUW,10     6
     O***************     39   DDU,10    25
     O***************  80 39 51AMT,10BB  44
      *** END DEL **** 06/14/02 ****************************************
     O          E            SUMST1         0 40
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMST          2
     O          E            SUMST          1
     O               39                          23 'Deposit             '
     O               39    51XXDEPA        BB    51
     O          E            SUMST          1
     O                 N45 39XXTHD               23
     O               39N45 51XXTXDU        BB    51
     O          E            SUMST          1
     O               39                          23 'Deposit Received    '
     O               39    51XXDPRL        BB    51
     O          E            SUMST          1
     O               41 39                       23 'Deposit Due *       '
     O               42 39                       23 'Credit *            '
     O                  39 51XXDPDU        BB    51
     O          E            SUMST          1
     O               39 52                       23 'Membership Dues     '
     O               39 52 51XXMFEE        BB    51
     O          E            SUMST          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 53             23 'Safety Fees Due     '
     O               39 53   XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 53 51XXSFEE        BB    51
     O          E            SUMST          1
     O               39 55   XX1STH              23
     O               39 55 51XX1STB        BB    51
     O          E            SUMST          1
     O               39 56   XX2NDH              23
     O               39 56 51XX2NDB        BB    51
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            SUMST          1
     O               39 64   XX3RDH              23
     O               39 64 51XX3RDB        BB    51
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            SUMST          1
     O               39 91   XX4THH              23
     O               39 91 51XX4THB        BB    51
     O          E            SUMST          1
     O               39 98   XX5THH              23
     O               39 98 51XX5THB        BB    51
     O*** END ADD *****  07/02/04  ************************************
     O          E            SUMST          1
     O               39N35                       23 'Expense Constant Due'
     O               39N35 51XXECDU        BB    51
     O          E    39    51SUMST          1
     O                                           49 '___________'
     O          E            SUMST          2
     O               39                          23 'Total Due           '
     O               39    51XXTLDU        BB    51 '$'
     O          E            SUMST          1
     O                     39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********              39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********              39XBDESC    48
     O                     39XBDESC              43
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O          E            SUMST          1
     O               43 72 39                    24 '* Payment Due No Later '
     O               43 72 39                    28 'Than'
     O               43 72 39XXDUED         B    39 '  /  /    '
     O                  73 39                    24 '* Please Deduct The Cre'
     O                  73 39                    43 'dit Indicated Above'
     O          E            SUMST          1
     O                  73 39                    24 '  From Your First Month'
     O                  73 39                    43 'ly Payroll Report. '
     O                       BCCOM1              75
     O          E            SUMST          1
     O                       BCCOM2              75
     O          E            SUMST          1
     O                       BCCOM3              75
     O*
     O          E            SUMAT1         0 40
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMAT          2
     O          E            SUMAT          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O**************                             23 'Audited Premium     '
     O              N25                          23 'Audited Premium     '
     O               25                          23 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     51
     O          E            SUMAT          1
     O               52                          23 'Membership Dues     '
     O               52    51XXMFEE        B     51
     O          E            SUMAT          1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        53                23 'Safety Fees Due     '
     O               53      XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               53    51XXSFEE        B     51
     O          E            SUMAT          1
     O               55      XX1STH              23
     O               55    51XX1STB        B     51
     O          E            SUMAT          1
     O               56      XX2NDH              23
     O               56    51XX2NDB        B     51
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            SUMAT          1
     O               64      XX3RDH              23
     O               64    51XX3RDB        B     51
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            SUMAT          1
     O               91      XX4THH              23
     O               91    51XX4THB        B     51
     O          E            SUMAT          1
     O               98      XX5THH              23
     O               98    51XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            SUMAT          1
     O               67                          23 'Installment Fees Due'
     O               67 51   XXINST        B     51
     O*** END ADD *****  03/04/02  ************************************
     O          E            SUMAT          1
     O                 N35                       23 'Expense Constant    '
     O                 N35 51XXEXCN        B     51
     O          E            SUMAT          1
     O                 N45   XXTHD               23
     O              N45 51   XXTXDU        A     51
     O*** BEG ADD *****  06/27/06  ************************************
     O          E            SUMAT          1
     O                       XXGHDG              23
     O                     51XXGAMT        B     51
     O*** END ADD *****  06/27/06  ************************************
     O          E          51SUMAT          0
     O                                           49 '___________'
     O          E            SUMAT          1
     O                                           23 'Previously Paid     '
     O                     51XXDPRL        B     51
     O          E            SUMAT          2
     O               41                          23 'Balance Due *       '
     O               42                          23 'Credit *            '
     O                     51XXDPDU        B     51 '$'
     O          E            SUMAT          1
     O               41N84                       24 '* Payment Due No Later '
     O               41N84                       28 'Than'
     O               41N84   XXDUED              39 '  /  /    '
     O          E            SUMAT          1
     O              N83 43                       24 '* Please Deduct The Cre'
     O              N83 43                       43 'dit Indicated Above'
     O          E            SUMAT          1
     O              N83 43                       24 '  From Your First Month'
     O              N83 43                       43 'ly Payroll Report. '
     O          E            SUMAT          1
     O                       BCCOM1              75
     O          E            SUMAT          1
     O                       BCCOM2              75
     O          E            SUMAT          1
     O                       BCCOM3              75
      *** BEGIN ADD *** 04/17/06 *************************************
     O          E            SUMAT#         1
     O                                           36 'Billed'
     O                                           54 'Paid'
     O                                           78 'Balance Due'
     O          E            SUMAT#         1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O****************                           20 'Audited Premium  '
     O              N25                          20 'Audited Premium  '
     O               25                          20 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     40
     O                     51XXPRPD        A     60
     O                     51XXPBAL        A     80
     O          E            SUMAT#         1
     O               52                          20 'Membership Dues  '
     O               52    51XXMFEE        B     40
     O               52    51XXMFPD        A     60
     O               52    51XXMBAL        A     80
     O          E            SUMAT#         1
     O               53                          20 'Safety Fees Due  '
     O               53    51XXSFEE        B     40
     O               53    51XXSFPD        A     60
     O               53    51XXSBAL        A     80
     O          E            SUMAT#         1
     O               55      XX1STH              20
     O*****MLD         55    51X196  4   30
     O**** MLD         55                31 '%'
     O               55    51XX1STB        B     40
     O               55    51XX1SPD        A     60
     O               55    51XX1BAL        A     80
     O          E            SUMAT#         1
     O               56      XX2NDH              20
     O*****MLD         56    51X296  4   30
     O*****MLD         56                31 '%'
     O               56    51XX2NDB        B     40
     O               56    51XX2NPD        A     60
     O               56    51XX2BAL        A     80
     O          E            SUMAT#         1
     O                 N35                       20 'Expense Constant '
     O                 N35 51XXEXCN        B     40
     O                 N35 51XXEXPD        A     60
     O                 N35 51XXEBAL        A     80
     O*** BEGIN ADD ***  06/05/08  *************************************
     O          E            SUMAT#         1
     O                       XXGHDG              23
     O                     51XXGAMT        A     40
     O                     51XXGPD         A     60
     O                     51XXFBAL        A     80
     O*** END ADD *****  06/05/08  *************************************
     O          E            SUMAT#         1
     O                 N28                       20 'NSF/Legal Fees   '
     O                 N28 51XXGFEE        B     40
     O                 N28 51XXGFPD        A     60
     O                 N28 51XXGBAL        A     80
     O          E            SUMAT#         0
     O              N57                          25 '________________________'
     O              N57                          48 '________________________'
     O              N57                          71 '________________________'
     O              N57                          80 '_________'
     O          E          51SUMAT#         1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************      N45   XXTHD     20
     O                 N45   XXTHD               23
     O*** END CHG *****  06/05/08  *************************************
     O              N45 51   ELTXDU        A     40
     O              N45 51   XXTAXP        A     60
     O              N45 51   XXTBAL        A     80
     O          E            SUMAT#         2
     O              N57                          20 'Total            '
     O              N57      XXTBIL        A     40
     O              N57      XXDPRL        A     60
     O              N57      XXDPDU        A     80
     O          E            SUMAT#         1
     O               41N84                       24 '* Payment Due No Later '
     O               41N84                       28 'Than'
     O               41N84   XXDUED              39 '  /  /    '
     O          E            SUMAT#         1
     O              N83 43                       24 '* Please Deduct The Cre'
     O              N83 43                       43 'dit Indicated Above'
     O          E            SUMAT#         1
     O              N83 43                       24 '  From Your First Month'
     O              N83 43                       43 'ly Payroll Report. '
     O          E            SUMAT#         1
     O                       BCCOM1              75
     O          E            SUMAT#         1
     O                       BCCOM2              75
     O          E            SUMAT#         1
     O                       BCCOM3              75
      *** END CHG ***** 04/17/06 *************************************
     O          E            SUMFT          1 64
     O***** beg chg ******* 11/22/13 ******************************************
     O*****                                      37 'Make checks payable to '
     O*****                  XXCKNM              67
     O               28                          37 'Make checks payable to '
     O               28      xxCkNm              67
     O***** end chg ******* 11/22/13 ******************************************
     O                                           76 'Page'
     O                       XXPAG4        Z     80
     OQSYSPRT   E            SUMH           1 01
     O               63N38   TIT(1)              56
     O              N63N38   TIT(2)              56
     O*** BEGIN ADD ***  10/21/03  ************************************
     O*********           38   TIT,3     56
     O              N30 38   TIT(3)              56
     O               30 38   TIT(4)              56
     O*** END ADD *****  10/21/03  ************************************
     O          E            SUMH           1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMH           1
     O                       XXFNNM              42
     O                                           58 'Print Date:   '
     O                       XXDATE              80 '  /  /    '
     O          E            SUMH           1
     O                       FNMAD1              42
     O**** BEGIN CHG **** 07/02/01 ************************************
     O****************** N38             58 'Policy #:     '
     O              N66N38                       58 'Policy #:     '
     O               66N38                       58 'File #:       '
     O**** END   CHG **** 07/02/01 ************************************
     O                  38                       58 'Quote #:      '
     O                       QQPOL#              80
     O          E            SUMH           1
     O              N65      FNMAD2              42
     O               65      FNCS                32
     O                                           58 'Division:     '
     O                       ELDIV               80
     O          E            SUMH           1
     O              N65      FNCS                32
     O                                           58 'Policy Period:'
     O                       XXFROM        Y     69
     O                                           70 '-'
     O                       XXTO          Y     80
     O          E            SUMH           1
     O               87                          68 'Anniversary Rating Date:'
     O               87      XXANNV        Y     80
     O          E            SUMH           1
     O                       EMN(1)              39
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            58 'Agent #:      '
     O*******                  AHAGT#Z   64
     O                       XXAHDR              60
     O                       AHAGT#        Z     80
     O*** END CHG *** 09/24/04 ************************************
     O          E            SUMH           1
     O                       EMN(2)              39
     O                       ANAME               80
     O          E            SUMH           1
     O                       EMN(3)              39
     O                       AADD1               80
     O          E            SUMH           1
     O                       EMN(4)              39
     O                       ACSZ                80
     O          E            SUMH           1
     O                       EMN(5)              39
     O               69                          47 '('
     O               69      AGARE1              50
     O               69                          51 ')'
     O               69      AGPHN1              60 '   -    '
     O          E            SUMH           1
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMH2          1
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********                           7 'Div'
     O*********                          10 'St'
     O*********                          16 'Code'
     O                                            6 'Div'
     O                                            9 'St'
     O                                           15 'Code'
     O*** END CHG *****  11/14/02  ************************************
     O                                           28 'Eff Date'
      **** BEGIN CHG **** 11/21/01 ************************************
     O********************************** 43 'Classification'
     O********************************** 62 'Payroll'
      **** beg chg ***** 01/24/11 *************************************
     O***********                                38 'Classification'
     O                                           43 'Classification'
      **** end chg ***** 01/24/11 *************************************
     O                                           57 'Payroll'
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********                         69 'Rate'
     O***********************            71 'Rate'
      **** END CHG ****** 09/24/01 ************************************
     O                                           66 'Rate'
      **** END   CHG **** 11/21/01 ************************************
      **** BEGIN CHG **** 11/21/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O******************                         80 'Premium'
     O              N25                          80 'Premium'
     O               25                          80 'Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O********************************** 85 'Premium'
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 11/21/01 ************************************
     O          E            SUMH2          2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
      **** BEGIN CHG **** 11/21/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O                                           80 '_________'
     O********************************** 85 '______________'
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 11/21/01 ************************************
     O          E            SUMD           1
     O*** BEGIN CHG ***  11/14/02  ************************************
     O*********                EPDIV Z    7
     O*********                XXST      10
     O*********                EPCLASZ   16
     O*********                EPSPEC    17
     O                       EPDIV         Z      6
     O                       XXST                 9
     O                       EPCLAS        Z     15
     O                       EPSPEC              17
     O*** BEG ADD *****  04/20/09  ************************************
     O*** BEG DEL ***** 01/24/11 **************************************
     O***********            EPEXT               23
     O*** END DEL ***** 01/24/11 **************************************
     O*** END ADD *****  04/20/09  ************************************
     O*** END CHG *****  11/14/02  ************************************
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            XPEFDT              28 '  /  /    '
     O*** BEG CHG ***** 01/24/11 **************************************
     O***********            XPEFDT              33 '  /  /    '
     O                       XPEFDT              28 '  /  /    '
     O*** END CHG ***** 01/24/11 **************************************
     O*** END CHG *****  04/20/09  ************************************
      **** BEGIN CHG **** 11/21/01 ************************************
     O************************ XXDESC    49
     O************************ XXEAPR2   62
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            XXDESC              44
     O***********            XXEAPR        2     57
     O*** BEG CHG ***** 01/24/11 **************************************
     O***********            XXDESC              49
     O***********            XXEAPR        2     62
     O                       XXDESC              44
     O                       XXEAPR        2     57
     O*** END CHG ***** 01/24/11 **************************************
     O*** END CHG *****  04/20/09  ************************************
      **** BEGIN CHG **** 09/24/01 ************************************
     O**********               EPARAT2   69
     O************************ EPARAT2   71
     O*** BEG CHG *****  04/20/09  ************************************
     O***********            EPARAT        2     66
     O*** BEG CHG ***** 01/24/11 **************************************
     O***********            EPARAT        2     69
     O                       EPARAT        2     66
     O*** END CHG ***** 01/24/11 **************************************
     O*** END CHG *****  04/20/09  ************************************
      **** END CHG ****** 09/24/01 ************************************
      **** BEGIN CHG **** 08/30/01 ************************************
     O                       XXEMPR        J     80
     O*** BEGIN ADD ****** 10/13/11 ************************************
     O                       XXEMSN              81
     O*** END   ADD ****** 10/13/11 ************************************
     O************************ XXEMPR1   85
      **** END   CHG **** 08/30/01 ************************************
      **** END   CHG **** 11/21/01 ************************************
     O*
     O          E            SUME1          0 39
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUME           2
     O          E            SUME           1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O************   63    39                    25 'Premium               '
     O************  N63    39                    25 'Revised Premium       '
     O               63N25 39                    25 'Premium               '
     O              N63N25 39                    25 'Revised Premium       '
     O               63 25 39                    25 'Contribution          '
     O              N63 25 39                    25 'Revised Contribution  '
     O*** END   CHG ****** 01/14/10 ************************************
     O                  51 39XXPRO         A     51
     O          E            SUME           1
     O               39 51 52                    25 'Membership Dues       '
     O               39 51 52XXMFDU        A     51
     O          E            SUME           1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 51 53          25 'Safety Fees Due       '
     O               39 51 53XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 51 53XXSFDU        B     51
     O          E            SUME           1
     O               39 51 55XX1STH              23
     O               39 51 55XX1STB        B     51
     O          E            SUME           1
     O               39 51 56XX2NDH              23
     O               39 51 56XX2NDB        B     51
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            SUME           1
     O               39 51 64XX3RDH              23
     O               39 51 64XX3RDB        B     51
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            SUME           1
     O               39 51 91XX4THH              23
     O               39 51 91XX4THB        B     51
     O          E            SUME           1
     O               39 51 98XX5THH              23
     O               39 51 98XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            SUME           1
     O               67    39                    25 'Installment Fees Due  '
     O               67 51 39XXINST        B     51
     O*** END ADD *****  03/04/02  ************************************
     O          E            SUME           1
     O              N44    39                    25 'Expense Constant      '
     O              N44 51 39XXEXCN        A     51
     O          E            SUME           1
     O                 N45 39XXTHD               23
     O              N45 51 39XXTXDU        B     51
     O          E       51   SUME           0
     O                     39                    49 '___________'
     O          E            SUME           1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O               63    39                    25 'Premium Paid          '
     O               63N25 39                    25 'Premium Paid          '
     O               63 25 39                    25 'Contribution Paid     '
     O*** END   CHG ****** 01/14/10 ************************************
     O              N63    39                    25 'Previously Paid       '
     O                  51 39XXDPRP        A     51
     O          E            SUME           2
     O                     39                    25 'Balance               '
     O                  51 39XXBAL         A     51 '$'
     O          E            SUME           1
     O                     39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********              39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********              39XBDESC    48
     O                     39XBDESC              43
     O               78 39   DUW(8)              48
     O                  39   DDU(8)              67
     O               78 39 51AMT(8)        B     80
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O          E            SUME           1
     O               71 39   DUW(1)               6
     O               63 39   DDU(1)              25
     O              N63 39 71                    11 'thru'
     O              N63 39   DDU(1)              30
     O               71 39 51AMT(1)        B     44
      *** BEG CHG **** 06/14/02 ****************************************
     O***************  81 39   DUW,11    48
     O***************     39   DDU,11    67
     O***************  81 39 51AMT,11B   80
     O*** BEGIN CHG ***  07/09/02  ************************************
     O*******          81 39   DUW,10    48
     O*******             39   DDU,10    67
     O*******          81 39 51AMT,10B   80
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*************    80 39   DUW,10    48
     O*************       39   DDU,10    67
     O*************    80 39 51AMT,10B   80
     O*** BEGIN CHG ***  06/12/06  ************************************
     O*************    79 39   DUW,9      6
     O*************       39   DDU,9     25
     O*************    79 39 51AMT,9 B   44
     O               79 39   DUW(9)              48
     O                  39   DDU(9)              67
     O               79 39 51AMT(9)        B     80
     O*** END   CHG ***  06/12/06  ************************************
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  07/09/02  ************************************
      *** END CHG **** 06/14/02 ****************************************
     O          E            SUME           1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        72 39   DUW,2      6
     O*********           39   DDU,2     25
     O               72 39N86DUW(2)               6
     O                  39N86DDU(2)              25
     O                  39 86ZZ            2      5
     O*** BEG CHG ***** 07/17/08 **************************************
     O***********         39 86          25 'Weekly Installments'
     O*** BEGIN ADD ***  04/25/03  ************************************
     O***********      36    86          30 'Installments on 1st/15th'
     O*** END ADD *****  04/25/03  ************************************
     O                  39 86INSTVB              30
     O*** END CHG ***** 07/17/08 **************************************
     O*** END CHG *****  04/21/03  ************************************
     O               72 39 51AMT(2)        B     44
      *** BEG CHG **** 06/14/02 ****************************************
     O***************  82 39   DUW,12    48
     O***************     39   DDU,12    67
     O***************  82 39 51AMT,12B   80
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*************    81 39   DUW,11    48
     O*************       39   DDU,11    67
     O*************    81 39 51AMT,11B   80
     O               80 39   DUW(10)             48
     O                  39   DDU(10)             67
     O               80 39 51AMT(10)       B     80
     O*** END CHG *****  07/02/04  ************************************
      *** END CHG **** 06/14/02 ****************************************
     O          E            SUME           1
     O*** BEGIN CHG ***  04/21/03  ************************************
     O*********        73 39   DUW,3      6
     O*********           39   DDU,3     25
     O               73 39N86DUW(3)               6
     O                  39N86DDU(3)              25
     O                  39 86                    11 'Starting'
     O                  39 86X1WKDT              22 '  /  /    '
     O                  39 86                    25 'of'
     O*** END CHG *****  04/21/03  ************************************
     O               73 39 51AMT(3)        B     44
      *** BEG ADD **** 06/14/02 ****************************************
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*************    82 39   DUW,12    48
     O*************       39   DDU,12    67
     O*************    82 39 51AMT,12B   80
     O               81 39   DUW(11)             48
     O                  39   DDU(11)             67
     O               81 39 51AMT(11)       B     80
     O*** END CHG *****  07/02/04  ************************************
      *** END ADD **** 06/14/02 ****************************************
     O          E            SUME           1
     O               74 39   DUW(4)               6
     O                  39   DDU(4)              25
     O               74 39 51AMT(4)        B     44
     O*** BEG ADD *****  07/02/04  ************************************
     O               82 39   DUW(12)             48
     O                  39   DDU(12)             67
     O               82 39 51AMT(12)       B     80
     O*** END ADD *****  07/02/04  ************************************
     O*** BEG DEL *****  07/02/04  ************************************
     O*****************        BCCOM1    75
     O*** END DEL *****  07/02/04  ************************************
     O          E            SUME           1
     O               75 39   DUW(5)               6
     O                  39   DDU(5)              25
     O               75 39 51AMT(5)        B     44
     O*** BEG CHG *****  07/02/04  ************************************
     O*****************        BCCOM2    75
     O                       BCCOM1              75
     O*** END CHG *****  07/02/04  ************************************
     O          E            SUME           1
     O               76 39   DUW(6)               6
     O                  39   DDU(6)              25
     O               76 39 51AMT(6)        B     44
     O*** BEG CHG *****  07/02/04  ************************************
     O*****************        BCCOM3    75
     O                       BCCOM2              75
     O*** END CHG *****  07/02/04  ************************************
     O          E            SUME           1
     O               77 39   DUW(7)               6
     O                  39   DDU(7)              25
     O               77 39 51AMT(7)        B     44
     O*** BEG ADD *****  07/02/04  ************************************
     O                       BCCOM3              75
     O*** END ADD *****  07/02/04  ************************************
      *** BEG DEL ***** 06/12/06 **************************************
     O********E**1*            SUME
     O*************    78 39   DUW,8      6
     O*************       39   DDU,8     25
     O*************    78 39 51AMT,8 B   44
     O********E**1*            SUME
     O*************    79 39   DUW,9      6
     O*************       39   DDU,9     25
     O*************    79 39 51AMT,9 B   44
      *** END DEL ***** 06/12/06 **************************************
      *** BEG DEL **** 06/14/02 ****************************************
     O********E**1***          SUME
     O***************  80 39   DUW,10     6
     O***************     39   DDU,10    25
     O***************  80 39 51AMT,10B   44
      *** END DEL **** 06/14/02 ****************************************
     O          E            SUMS1          0 39
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMS           2
     O          E            SUMS           1
     O               39                          23 'Deposit             '
     O               39    51XXDEPA        B     51
     O          E            SUMS           1
     O                 N45 39XXTHD               23
     O               39N45 51XXTXDU        B     51
     O          E            SUMS           1
     O               39                          23 'Deposit Received    '
     O               39    51XXDPRL        B     51
     O          E            SUMS           1
     O               41 39                       23 'Deposit Due *       '
     O               42 39                       23 'Credit *            '
     O                  39 51XXDPDU        B     51
     O          E            SUMS           1
     O               39 52                       23 'Membership Dues     '
     O               39 52 51XXMFEE        B     51
     O          E            SUMS           1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        39 53             23 'Safety Fees Due     '
     O               39 53   XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               39 53 51XXSFEE        B     51
     O          E            SUMS           1
     O               39 55   XX1STH              23
     O               39 55 51XX1STB        B     51
     O          E            SUMS           1
     O               39 56   XX2NDH              23
     O               39 56 51XX2NDB        B     51
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            SUMS           1
     O               39 64   XX3RDH              23
     O               39 64 51XX3RDB        B     51
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            SUMS           1
     O               39 91   XX4THH              23
     O               39 91 51XX4THB        B     51
     O          E            SUMS           1
     O               39 98   XX5THH              23
     O               39 98 51XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O          E            SUMS           1
     O               39N35                       23 'Expense Constant Due'
     O               39N35 51XXECDU        B     51
     O          E    39    51SUMS           1
     O                                           49 '___________'
     O          E            SUMS           2
     O               39                          23 'Total Due           '
     O               39    51XXTLDU        B     51 '$'
     O          E            SUMS           1
     O                     39                    18 'Payment Plan:  '
     O*** BEGIN CHG ***  09/14/01  ************************************
     O*********              39XBDESC    43
     O*** BEGIN CHG ***  07/02/04  ************************************
     O*********              39XBDESC    48
     O                     39XBDESC              43
     O*** END CHG *****  07/02/04  ************************************
     O*** END CHG *****  09/14/01  ************************************
     O          E            SUMS           1
     O               43 72 39                    24 '* Payment Due No Later '
     O               43 72 39                    28 'Than'
     O               43 72 39XXDUED              39 '  /  /    '
     O                  73 39                    24 '* Please Deduct The Cre'
     O                  73 39                    43 'dit Indicated Above'
     O          E            SUMS           1
     O                  73 39                    24 '  From Your First Month'
     O                  73 39                    43 'ly Payroll Report. '
     O                       BCCOM1              75
     O          E            SUMS           1
     O                       BCCOM2              75
     O          E            SUMS           1
     O                       BCCOM3              75
     O*
     O          E            SUMA1          0 39
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          E            SUMA           2
     O          E            SUMA           1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O***************                            23 'Audited Premium     '
     O                                           23 'Audited Premium     '
     O                                           23 'Audited Contribution'
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     51
     O          E            SUMA           1
     O               52                          23 'Membership Dues     '
     O               52    51XXMFEE        B     51
     O          E            SUMA           1
     O*** BEGIN CHG ***  11/20/01  ************************************
     O*********        53                23 'Safety Fees Due     '
     O               53      XXSFHD              23
     O*** END CHG *****  11/20/01  ************************************
     O               53    51XXSFEE        B     51
     O          E            SUMA           1
     O               55      XX1STH              23
     O               55    51XX1STB        B     51
     O          E            SUMA           1
     O               56      XX2NDH              23
     O               56    51XX2NDB        B     51
     O*** BEGIN ADD ***  01/02/01  ************************************
     O          E            SUMA           1
     O               64      XX3RDH              23
     O               64    51XX3RDB        B     51
     O*** END ADD *****  01/02/01  ************************************
     O*** BEGIN ADD ***  07/02/04  ************************************
     O          E            SUMA           1
     O               91      XX4THH              23
     O               91    51XX4THB        B     51
     O          E            SUMA           1
     O               98      XX5THH              23
     O               98    51XX5THB        B     51
     O*** END ADD *****  07/02/04  ************************************
     O*** BEGIN ADD ***  03/04/02  ************************************
     O          E            SUMA           1
     O               67                          23 'Installment Fees Due'
     O               67 51   XXINST        B     51
     O*** END ADD *****  03/04/02  ************************************
     O          E            SUMA           1
     O                 N35                       23 'Expense Constant    '
     O                 N35 51XXEXCN        B     51
     O          E            SUMA           1
     O                 N45   XXTHD               23
     O              N45 51   XXTXDU        A     51
     O*** BEG ADD *****  06/27/06  ************************************
     O          E            SUMA           1
     O                       XXGHDG              23
     O                     51XXGAMT        B     51
     O*** END ADD *****  06/27/06  ************************************
     O          E          51SUMA           0
     O                                           49 '___________'
     O          E            SUMA           1
     O                                           23 'Previously Paid     '
     O                     51XXDPRL        B     51
     O          E            SUMA           2
     O               41                          23 'Balance Due *       '
     O               42                          23 'Credit *            '
     O                     51XXDPDU        B     51 '$'
     O          E            SUMA           1
     O               41N84                       24 '* Payment Due No Later '
     O               41N84                       28 'Than'
     O               41N84   XXDUED              39 '  /  /    '
     O          E            SUMA           1
     O              N83 43                       24 '* Please Deduct The Cre'
     O              N83 43                       43 'dit Indicated Above'
     O          E            SUMA           1
     O              N83 43                       24 '  From Your First Month'
     O              N83 43                       43 'ly Payroll Report. '
     O          E            SUMA           1
     O                       BCCOM1              75
     O          E            SUMA           1
     O                       BCCOM2              75
     O          E            SUMA           1
     O                       BCCOM3              75
      *** BEGIN ADD *** 04/17/06 *************************************
     O          E            SUMA#          1
     O                                           36 'Billed'
     O                                           54 'Paid'
     O                                           78 'Balance Due'
     O          E            SUMA#          1
     O*** BEGIN CHG ****** 01/14/10 ************************************
     O*******************                        20 'Audited Premium  '
     O              N25                          20 'Audited Premium  '
     O               25                          20 'Audited Premium  '
     O*** END   CHG ****** 01/14/10 ************************************
     O                     51XXPRO         B     40
     O                     51XXPRPD        A     60
     O                     51XXPBAL        A     80
     O          E            SUMA#          1
     O               52                          20 'Membership Dues  '
     O               52    51XXMFEE        B     40
     O               52    51XXMFPD        A     60
     O               52    51XXMBAL        A     80
     O          E            SUMA#          1
     O               53                          20 'Safety Fees Due  '
     O               53    51XXSFEE        B     40
     O               53    51XXSFPD        A     60
     O               53    51XXSBAL        A     80
     O          E            SUMA#          1
     O               55      XX1STH              20
     O*****MLD         55    51X196  4   30
     O*****MLD         55                31 '%'
     O               55    51XX1STB        B     40
     O               55    51XX1SPD        A     60
     O               55    51XX1BAL        A     80
     O          E            SUMA#          1
     O               56      XX2NDH              20
     O*****MLD         56    51X296  4   30
     O*****MLD         56                31 '%'
     O               56    51XX2NDB        B     40
     O               56    51XX2NPD        A     60
     O               56    51XX2BAL        A     80
     O          E            SUMA#          1
     O                 N35                       20 'Expense Constant '
     O                 N35 51XXEXCN        B     40
     O                 N35 51XXEXPD        A     60
     O                 N35 51XXEBAL        A     80
     O*** BEGIN ADD ***  06/05/08  *************************************
     O          E            SUMA#          1
     O                       XXGHDG              23
     O                     51XXGAMT        A     40
     O                     51XXGPD         A     60
     O                     51XXFBAL        A     80
     O*** END ADD *****  06/05/08  *************************************
     O          E            SUMA#          1
     O                 N28                       20 'NSF/Legal Fees   '
     O                 N28 51XXGFEE        B     40
     O                 N28 51XXGFPD        A     60
     O                 N28 51XXGBAL        A     80
     O          E            SUMA#          0
     O              N57                          25 '________________________'
     O              N57                          48 '________________________'
     O              N57                          71 '________________________'
     O              N57                          80 '_________'
     O          E          51SUMA#          1
     O*** BEGIN CHG ***  06/05/08  *************************************
     O*************      N45   XXTHD     20
     O                 N45   XXTHD               23
     O*** END CHG *****  06/05/08  *************************************
     O              N45 51   ELTXDU        A     40
     O              N45 51   XXTAXP        A     60
     O              N45 51   XXTBAL        A     80
     O          E            SUMA#          2
     O              N57                          20 'Total            '
     O              N57      XXTBIL        A     40
     O              N57      XXDPRL        A     60
     O              N57      XXDPDU        A     80
     O          E            SUMA#          1
     O               41N84                       24 '* Payment Due No Later '
     O               41N84                       28 'Than'
     O               41N84   XXDUED              39 '  /  /    '
     O          E            SUMA#          1
     O              N83 43                       24 '* Please Deduct The Cre'
     O              N83 43                       43 'dit Indicated Above'
     O          E            SUMA#          1
     O              N83 43                       24 '  From Your First Month'
     O              N83 43                       43 'ly Payroll Report. '
     O          E            SUMA#          1
     O                       BCCOM1              75
     O          E            SUMA#          1
     O                       BCCOM2              75
     O          E            SUMA#          1
     O                       BCCOM3              75
      *** END CHG ***** 04/17/06 *************************************
     O          E            SUMF           1 63
     O***** beg chg ******* 11/22/13 ******************************************
     O*****                                      37 'Make checks payable to '
     O*****                  XXCKNM              67
     O               28                          37 'Make checks payable to '
     O               28      xxCkNm              67
     O***** end chg ******* 11/22/13 ******************************************
     O                                           76 'Page'
     O*** BEGIN CHG ***  03/24/00  ************************************
     O*************            XXPAG4Z   80
     O                       XXPAGE        Z     80
     O*** END CHG *****  03/24/00  ************************************
     O*** END ADD *****  11/15/99  ************************************
