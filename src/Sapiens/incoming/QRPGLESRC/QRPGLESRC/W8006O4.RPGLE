      * Policy year information
     O          E            head4       2  1
     O                                            6 'Policy'
     O                                         +002 'Carrier                  '
     O                                         +002 '        Prior  '
     O                                         +002 '       Current '
     O                                         +002 'Payroll % '
     O                                         +002 '         Prior '
     O                                         +002 '       Current '
     O                                         +002 '         Total '
     O          E            head4          1
     O                                            6 ' Year '
     O                                         +002 'Policy #                 '
     O                                         +002 '       Payroll '
     O                                         +002 '       Payroll '
     O                                         +002 '          '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +002 '        Billed '
     O                                         +001 '*'
      * Detail
     O          E            detail4     1
     O                       FDFYR         z   +002
     O                       FDCAP#            +003
     O                       FDPRPY        j   +003
     O                       FDCUPY        j   +002
     O                       FDCPCT        j   +003
     O                       FDPRBL        j   +003
     O                       FDCUBL        j   +002
     O                       TotBill4      j   +002
      *
     O          E            total4a     3
