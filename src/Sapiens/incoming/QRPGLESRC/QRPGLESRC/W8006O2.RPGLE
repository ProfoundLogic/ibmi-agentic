      * Claim information
     O          E            head2       2  1
     O                                            7 'Claim #'
     O                                         +002 'Claimant Name       '
     O                                         +000 '                    '
     O                                         +002 '          '
     O                                         +002 'Accident Dt/'
     O                                         +002 '        '
     O                                         +002 '               '
     O                                         +002 '               '
     O                                         +002 '               '
     O                                         +002 '               '
     O                                         +002 'O/C'
     O          E            head2          1
     O                                            7 '       '
     O                                         +002 'Soc Sec #           '
     O                                         +000 '                    '
     O                                         +002 ' Occ Code '
     O                                         +002 'Report Date '
     O                                         +002 '        '
     O                                         +002 '  Compensation '
     O                                         +002 '       Medical '
     O                                         +002 '       Expense '
     O                                         +002 '         Total '
     O                                         +002 '   '
      * Detail
     O          E            detail2     1
     O                       FDCASE        z      7
     O                       FDCNAM            +002
     O                                         +002 '          '
     O                       FDACDT            +003 '  /  /    '
     O                                         +003 'Incurred'
     O                       FDCINC        j   +002
     O                       FDMINC        j   +002
     O                       FDEINC        j   +002
     O                       FDTINC        j   +002
     O                       FDSTAT            +003
     O          E            detail2     1  1
     O                                            7 '       '
     O                       xxSS#             +002
     O                                         +000 '         '
     O                                         +000 '                    '
     O                       FDOCCD            +002
     O                       FDDTRP            +003 '  /  /    '
     O                                         +003 'Paid    '
     O                       FDCPD         j   +002
     O                       FDMPD         j   +002
     O                       FDEPD         j   +002
     O                       FDTPD         j   +002
      *
     O          E            total2p     3
     O                                              'Total Claims Paid'
     O                                         +001 'to date for Policy Year'
     O                                         +001 '('
     O                       xxFrom            +000 '  /  /    '
     O                                         +001 'thru'
     O                       xxTo              +001 '  /  /    '
     O                                         +000 ')'
     O          E            total2p     1
     O                                              '         Prior '
     O                                         +002 '       Current '
     O                                         +002 ' Claims Pd %'
     O                                         +002 '         Prior '
     O                                         +002 '       Current '
     O                                         +002 '         Total '
     O          E            total2p     1
     O                                              '          Paid '
     O                                         +002 '         Paid  '
     O                                         +002 '            '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +001 '*'
     O          E            total2p     1
     O                       PriPaid       j
     O                       CurPaid       j   +002
     O                       ClmPdPct      j   +005
     O                       PriBillP      j   +004
     O                       CurBillP      j   +002
     O                       BilledP       j   +002
     O          E            total2p     2
     O                                              '* Amount may represent'
     O                                         +001 'the miniumum charge'
     O                                         +001 'and not based'
     O                                         +001 'specfically on'
     O                                         +001 'paid.'
      *
     O          E            total2i     3
     O                                              'Total Claims Incurred'
     O                                         +001 'to date for Policy Year'
     O                                         +001 '('
     O                       xxFrom            +000 '  /  /    '
     O                                         +001 'thru'
     O                       xxTo              +001 '  /  /    '
     O                                         +000 ')'
     O          E            total2i     1
     O                                              '         Prior '
     O                                         +002 '       Current '
     O                                         +002 'Claims Inc %'
     O                                         +002 '         Prior '
     O                                         +002 '       Current '
     O                                         +002 '         Total '
     O          E            total2i     1
     O                                              '      Incurred '
     O                                         +002 '      Incurred '
     O                                         +002 '            '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +001 '*'
     O          E            total2i     1
     O                       PriInc        j
     O                       CurInc        j   +002
     O                       ClmIncPct     j   +005
     O                       PriBillI      j   +004
     O                       CurBillI      j   +002
     O                       BilledI       j   +002
     O          E            total2i     2
     O                                              '* Amount may represent'
     O                                         +001 'the miniumum charge'
     O                                         +001 'and not based'
     O                                         +001 'specfically on'
     O                                         +001 'incurred.'
