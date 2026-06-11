      * Policy year information
     O          E            head3       2  1
     O                                              'Policy'
     O                                         +002 'Carrier                  '
     O                                         +002 '        Prior  '
     O                                         +002 '       Current '
     O                                         +002 'Prem % '
     O                                         +002 '         Prior '
     O                                         +002 '       Current '
     O                                         +002 '         Total '
     O          E            head3          1
     O                                              ' Year '
     O                                         +002 'Policy #                 '
     O                                         +002 '       Premium '
     O                                         +002 '       Premium '
     O                                         +002 '       '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +001 '*'
      * Detail
     O          E            detail3     1
     O                       FDFYR         z   +002
     O                       FDCAP#            +003
     O                       FDPRPR        j   +003
     O                       FDCUPR        j   +002
     O                       FDCPCT        j   +002
     O                       FDPRBL        j   +002
     O                       FDCUBL        j   +002
     O                       TotBill3      j   +002
      *
     O          E            total3b     2
     O                                              '* Amount may represent'
     O                                         +001 'the miniumum charge'
     O                                         +001 'and not based'
     O                                         +001 'specfically on'
     O                                         +001 'incurred.'
