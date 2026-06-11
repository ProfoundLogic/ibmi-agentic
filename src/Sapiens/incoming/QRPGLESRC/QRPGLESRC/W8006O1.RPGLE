      * Claim information
     O          E            head1       2  0
     O                       xxNewOrChgd
     O          E            head1          1
     O                                          181 '  Previous '
     O                                          197 '  Current  '
     O          E            head1          1
     O                                            7 'Claim #'
     O                                           33 'Claimant Name'
     O                                           72 'Accident Dt/'
     O                                          167 'O/C'
     O                                          181 'Designation'
     O                                          197 'Designation'
     O          E            head1          1
     O                                           24 'SS #'
     O                                           48 'Occ Code'
     O                                           72 'Report Date '
     O                                          102 'Compensation'
     O                                          122 'Medical'
     O                                          142 'Expense'
     O                                          162 'Total'
     O                                          181 'I/M/R Date '
     O                                          197 'I/M/R Date '
      * Detail
     O          E            detail1     1
     O                       FDCASE        z      7
     O                       FDCNAM              59
     O                       FDACDT              70 '  /  /    '
     O                                           87 'Incurred'
     O                       FDCINC        j    102
     O                       FDMINC        j    122
     O                       FDEINC        j    142
     O                       FDTINC        j    162
     O                       FDSTAT             166
     O                       FDPRTY             176
     O                       FDCUTY             192
     O          E            detail1     1  1
     O                       xxSS#               31
     O                       FDOCCD              48
     O                       FDDTRP              70 '  /  /    '
     O                                           87 'Paid    '
     O                       FDCPD         j    102
     O                       FDMPD         j    122
     O                       FDEPD         j    142
     O                       FDTPD         j    162
     O                       FDPRDT             180 '  /  /    '
     O                       FDCUDT             196 '  /  /    '
      *
     O          E            total1a     3
     O                       total1aHd
     O* * *                                         'Totals for New Claims'
     O* * *                                    +001 ' - Employer'
     O* * *                  saveFDEMP#    z   +001
     O* * *                  saveFDDIV     3   +001
     O* * *                  saveFDFYR     z   +001
     O          E            total1a     1
     O                                           73 '# of Claims'
     O                                           90 ' Fee Amt  '
     O                                          114 'Total Claims Fee '
     O          E            total1a     1
     O                                           90 'Per Claim '
     O          E            total1a     1
     O                                         +006 'New Indemnity  '
     O                       NewInd#       z     71
     O                       NewIndFee     j     90
     O                       NewIndTot     j    114
     O          E            total1a     1
     O                                         +006 'New Medical    '
     O                       NewMed#       z     71
     O                       NewMedFee     j     90
     O                       NewMedTot     j    114
     O          E            total1a     1
     O                                         +006 'New Record Only'
     O                       NewRcd#       z     71
     O                       NewRcdFee     j     90
     O                       NewRcdTot     j    114
     O          E            total1a     2
      *      'Total New ' + GTDESC
     O                       xxFeeDescHdg      +006
     O                       New#          z     71
     O                       NewTot        j    114
      *
     O          E            total1b     3
     O                       total1bHd
     O* * *                                         'Totals for Changed Claims'
     O* * *                                    +001 ' - Employer'
     O* * *                  saveFDEMP#    z   +001
     O* * *                  saveFDDIV     3   +001
     O* * *                  saveFDFYR     z   +001
     O          E            total1b     1
     O                                           73 '# of Claims'
     O                                           92 'Previous Fee '
     O                                          109 ' Current Fee '
     O                                          133 'Total Fee Billing'
     O          E            total1b     1
     O                                           92 '   Amount    '
     O                                          109 '   Amount    '
     O                                          133 '    Adjustment   '
     O          E            total1b     1
     O                                         +006 'Total Changed from I to M'
     O                       ChgIM#        z     71
     O                       ChgIMPrvFee   j     92
     O                       ChgIMCurFee   j    109
     O                       ChgIMAdj      j    132
     O          E            total1b     1
     O                                         +006 'Total Changed from I to R'
     O                       ChgIR#        z     71
     O                       ChgIRPrvFee   j     92
     O                       ChgIRCurFee   j    109
     O                       ChgIRAdj      j    132
     O          E            total1b     1
     O                                         +006 'Total Changed from M to I'
     O                       ChgMI#        z     71
     O                       ChgMIPrvFee   j     92
     O                       ChgMICurFee   j    109
     O                       ChgMIAdj      j    132
     O          E            total1b     1
     O                                         +006 'Total Changed from M to R'
     O                       ChgMR#        z     71
     O                       ChgMRPrvFee   j     92
     O                       ChgMRCurFee   j    109
     O                       ChgMRAdj      j    132
     O          E            total1b     1
     O                                         +006 'Total Changed from R to I'
     O                       ChgRI#        z     71
     O                       ChgRIPrvFee   j     92
     O                       ChgRICurFee   j    109
     O                       ChgRIAdj      j    132
     O          E            total1b     1
     O                                         +006 'Total Changed from R to M'
     O                       ChgRM#        z     71
     O                       ChgRMPrvFee   j     92
     O                       ChgRMCurFee   j    109
     O                       ChgRMAdj      j    132
     O          E            total1b     2
      *      'Total ' + GTDESC + ' Adjustments'
     O                       xxFeeDescHdg      +006
     O                       Chg#          z     71
     O                       ChgPrv        j     92
     O                       ChgCur        j    109
     O                       ChgAdj        j    132
      *
     O          E            totalFinal  2
     O                                           24 '***** FINAL TOTALS *****'
