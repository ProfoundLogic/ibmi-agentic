     O*****************************************************************
     O* REVISIONS:                                                    *
     O*                                                               *
     O* 06/07/18 C:65  Project: WC2607                                *
     O*                Move output specs to own source file for space.*
     O*                                                               *
     O*****************************************************************
      *====================================================================
     Oqsysprt   e   n44      form             04
     O          e   n44      form           1
     O              n66                          28 'WORKERS COMPENSATION AND'
     O              n66                          52 ' EMPLOYERS LIABILITY INS'
     O              n66                          76 'URANCE POLICY           '
     O               66                          51 'WORKERS COMPENSATION AND'
     O               66                          75 ' EMPLOYERS LIABILITY INS'
     O               66                          99 'URANCE POLICY           '
     O          e   n44      form           2
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                           94 '__________________'
     O          e    47      form           4
     Oqsysprt   e    44      nopdf            01
     Oqsysprt   e    44      pdf              05
     O          e    44      form           2
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                           94 '__________________'
     O          e            form           2
     O               24                          11 'REISSUE'
     O                                           57 'INFORMATION PAGE'
     O               25N81                       94 'RENEWAL AGREEMENT'
     O          e            form           1
     O                                           12 'Insurer:'
     O                       XHPROD              74
     O                       xxahdr              85
     O                       shagt#        z     91
     O          e            form           1
     O                       fnname              64
     O              n31      xxagnm              99
     O          e    31      form           1
     O                                           19 'A Stock Company'
     O                       xxagnm              99
     O          e            form           1
     O                       fnmad1              44
     O                       xxagad              99
     O          e    20      form           1
     O                       fnmad2              44
     O                       xxacsz              99
     O          e            form           1
     O                       xxfcsz              44
     O              n20      xxacsz              99
     O               20      XXPEOH              97
     O          e            form           1
     O                 N81                       18 '(Carrier Code:'
     O                 N81   f2car#              24
     O                 N81                       25 ')'
     O                 N81                       76 'Carrier Policy #:'
     O                  81                       21 '(NCCI Company No.'
     O                  81   f2car#              27
     O                  81                       28 ')'
     O                  81                       76 'Policy No.:'
     O                       xpol                95
     O          e            form           1
     O               56                          28 'N.J. Taxpayer Identifica'
     O               56                          37 'tion No.:'
     O               56      xxtxid              53
     O                 N81                       76 'Carrier Prior Policy #:'
     O                  81                       76 'Prior Policy No.:'
     O                       xppol               95
     O          e      n20   form           1
     O                       xxpeoh              97
     O          e            form           1
     O                                           20 '1.  The Insured:'
     O                       xxamn1              86
     O          e            form           1
     O                       xxadh2              24
     O                       xxamn2              86
     O          e            form           1
     O                       xxadh3              24
     O                       xxamn3              86
     O          e            form           1
     O                       xxadh4              24
     O                       xxamn4              86
     O          e            form           1
     O                       xxadh5              24
     O                       xxamn5              86
     O                 N81   x5fein              86
     O          e            form           1
     O                       xxamn6              86
     O          e            form           1
     O                       xxamn7              66
     O               80N81                       76 'File #:'
     O               80N81   xxfnd               80
     O               80N81   xxemp#              89
     O               80N81   xxfyr               92
     O          e      N74   form           1
     O                       xxamn8              66
     O                 N81   x7fein              86
     O          e       72   form           1
     O                       xxamn9              66
     O          e            form           1
     O                                           24 'Other workplaces'
     O                                           41 'not shown above:'
     O                                           76 'Type of Business:'
     O                       xxbtyp              97
     O          e            form           2
     O               23N81                       33 'SEE SCHEDULE OF OPERATI'
     O               23N81                       36 'ONS'
     O               23 81                       33 'See Extension of Inform'
     O               23 81                       43 'ation Page'
     O              n23                          33 'NO OTHER WORKPLACES FOR '
     O              n23                          44 'THIS POLICY'
     O                 N81                       76 'Risk ID:'
     O                 N81   xxRisk              92
     O          e            form           0
     O                       polPeriod1
     O          e            form           1
     O                       polPeriodU
     O          e            form           1
     O                       polPeriod2
     O          e            form           1
     O                       polPeriod3
     O          e            form           1
     O                                           28 '3.  A.  Workers Compensa'
     O                                           52 'tion Insurance: Part One'
     O                                           76 ' of the policy applies t'
     O                                           94 'o the Workers     '
     O          e            form           1
     O                                           28 '        Compensation Law'
     O                                           52 ' of the states listed he'
     O                                           76 're:                     '
     O          e            form           1
     O                       xst(1)              19
     O                       xst(2)              22
     O                       xst(3)              25
     O                       xst(4)              28
     O                       xst(5)              31
     O                       xst(6)              34
     O                       xst(7)              37
     O                       xst(8)              40
     O                       xst(9)              43
     O                       xst(10)             46
     O                       xst(11)             49
     O                       xst(12)             52
     O                       xst(13)             55
     O                       xst(14)             58
     O                       xst(15)             61
     O                       xst(16)             64
     O                       xst(17)             67
     O                       xst(18)             70
     O                       xst(19)             73
     O                       xst(20)             76
     O                       xst(21)             79
     O                       xst(22)             82
     O                       xst(23)             85
     O                       xst(24)             88
     O                       xst(25)             91
     O          e            form           1
     O                       xst(26)             19
     O                       xst(27)             22
     O                       xst(28)             25
     O                       xst(29)             28
     O                       xst(30)             31
     O                       xst(31)             34
     O                       xst(32)             37
     O                       xst(33)             40
     O                       xst(34)             43
     O                       xst(35)             46
     O                       xst(36)             49
     O                       xst(37)             52
     O                       xst(38)             55
     O                       xst(39)             58
     O                       xst(40)             61
     O                       xst(41)             64
     O                       xst(42)             67
     O                       xst(43)             70
     O                       xst(44)             73
     O                       xst(45)             76
     O                       xst(46)             79
     O                       xst(47)             82
     O                       xst(48)             85
     O                       xst(49)             88
     O                       xst(50)             91
     O          e            form           1
     O                                           28 '    B.  Employers Liabil'
     O                                           52 'ity Insurance: Part Two '
     O                                           76 'of the policy applies to'
     O                                           94 ' work in each     '
     O          e            form           1
     O                                           28 '        state listed in '
     O                                           52 'Item 3.A.  The limits of'
     O                                           76 ' our liability under Par'
     O                                           94 't Two are:        '
     O          e            form           1
     O          e            form           0
     O                                           41 'Bodily Injury by Acciden'
     O                                           46 't   $'
     O                       xxacc         j     61
     O                                           80 'each accident'
     O          e            form           1
     O                                           66 '____________________'
     O          e            form           0
     O                                           41 'Bodily Injury by Disease'
     O                                           46 '    $'
     O                       xxagg         j     61
     O                                           80 'policy limit '
     O          e            form           1
     O                                           66 '____________________'
     O          e            form           0
     O                                           41 'Bodily Injury by Disease'
     O                                           46 '    $'
     O                       xxdis         j     61
     O                                           80 'each employee'
     O          e            form           1
     O                                           66 '____________________'
     O          e   n97      form           1
     O          e    97      form           1
     O                                           31 'Liability limits'
     O                                         +  1 'effective'
     O                       x8i2ef            +  1 '  /  /    '
     O                                         +  0 ':'
     O          e    97      form           0
     O                                           41 'Bodily Injury by Acciden'
     O                                           46 't   $'
     O                       x2acc         j     61
     O                                           80 'each accident'
     O          e    97      form           1
     O                                           66 '____________________'
     O          e    97      form           0
     O                                           41 'Bodily Injury by Disease'
     O                                           46 '    $'
     O                       x2agg         j     61
     O                                           80 'policy limit '
     O          e    97      form           1
     O                                           66 '____________________'
     O          e    97      form           0
     O                                           41 'Bodily Injury by Disease'
     O                                           46 '    $'
     O                       x2dis         j     61
     O                                           80 'each employee'
     O          e    97      form           2
     O                                           66 '____________________'
     O          e            form           1
     O                                           32 'C.  Other States Insuran'
     O                                           35 'ce:'
     O                       xxpp3c              96
     O          e            form           2
     O                       xxppc3              96
     O          e            form           1
     O                                           28 '    D.  This policy incl'
     O                                           52 'udes these endorsements '
     O                                           76 'and schedules:          '
     O          e   n95      form           1
     O                       xf(1)               28
     O                       xf(2)               45
     O                       xf(3)               62
     O                       xf(4)               79
     O                       xf(5)               96
     O          e   n95      form           1
     O                       xf(6)               28
     O                       xf(7)               45
     O                       xf(8)               62
     O                       xf(9)               79
     O                       xf(10)              96
     O          e   n95      form           1
     O                       xf(11)              28
     O                       xf(12)              45
     O                       xf(13)              62
     O                       xf(14)              79
     O                       xf(15)              96
     O          e   n95      form           1
     O                       xf(16)              28
     O                       xf(17)              45
     O                       xf(18)              62
     O                       xf(19)              79
     O                       xf(20)              96
     O          e    95      form        1  2
     O                       ddtl                55
     O          e            form           1
     O               58                          28 '4.  The premium for this'
     O              N58                          28 '4.  The cntrbtn for this'
     O                                           52 ' policy will be determin'
     O                                           76 'ed by our Manuals of Rul'
     O                                           94 'es,               '
     O          e            form           1
     O                                           28 '    Classifications, Rat'
     O                                           52 'es and Rating Plans.  Al'
     O                                           76 'l information required b'
     O                                           94 'elow is subject   '
     O          e            form           1
     O                                           28 '    to verification and '
     O                                           52 'change by audit.        '
     O          e   n72      form           1
     O          e            form           1
     O                                           23 'Classifications'
     O                                           35 'Code'
     O               58                          59 'Premium Basis'
     O              N58                          59 'Cntrbtn Basis'
     O                                           77 'Rate Per'
     O                                           94 'Estimated'
     O          e            form           1
     O                                           35 'No.'
     O                                           60 'Total Estimated'
     O                                           77 '$100 of'
     O                                           93 'Annual'
     O          e            form           1
     O                                           62 'Annual Remuneration'
     O                                           79 'Remuneration'
     O               58                          93 'Premium'
     O              N58                          93 'Cntrbtn'
     O          e            form           1
     O          e            form           1
     O                 N81                       37 'SEE SCHEDULE OF OPERATI'
     O                 N81                       40 'ONS'
     O                  81                       37 'See Extension of Inform'
     O                  81                       47 'ation Page'
     O          e            form           1
     O          e            form           1
     O                 N81                       29 'Total Estimated Annual '
     O               58N81                       38 'Premium $'
     O              N58N81                       38 'Cntrbtn $'
     O                  81                       32 'Total Estimated Annual '
     O                  81                       38 'Cost $'
     O                       xxprm         1     52
     O               43 58                       79 'Prorated Premium $'
     O               43N58                       79 'Prorated Cntrbtn $'
     O               43      xxpror        1     93
     O          e    46      form           1
     O               58                          23 'Minimum Premium $'
     O              N58                          23 'Minimum Cntrbtn $'
     O                       xxmnpr        1     36
     O                 N81                       56 'Expense Constant $'
     O                 N81   xxexcn        1     67
     O               58 54                       86 'Deposit Premium $'
     O              N58 54                       86 'Deposit Cntrbtn $'
     O                  54   xxdep         1     99
     O          e    46 81   form           1
     O                                           21 'Servicing Office:'
     O                       xxofad              63
     O                                           79 'Date:'
     O                       xxdate              93 '  /  /    '
     O          e   n46N81   form           3
     O               58                          23 'Minimum Premium $'
     O              N58                          23 'Minimum Cntrbtn $'
     O                       xxmnpr        1     36
     O                                           56 'Expense Constant $'
     O                       xxexcn        1     67
     O               54 58                       86 'Deposit Premium $'
     O               54N58                       86 'Deposit Cntrbtn $'
     O               54      xxdep         1     99
     O          e   n46 81   form           1
     O               58                          38 'Minimum Premium $'
     O              N58                          38 'Minimum Cntrbtn $'
     O                       xxmnpr        1     52
     O               58                          79 'Deposit Premium $'
     O              N58                          79 'Deposit Cntrbtn $'
     O                       xxdep         1     93
     O          e   n46 81   form           2
     O                                           21 'Servicing Office:'
     O                       xxofad              63
     O                                           79 'Date:'
     O                       xxdate              93 '  /  /    '
     O          e   n46      form           1
     O                       xxDecForm           17
     O                                           54 'Countersigned by'
     O                                           78 '_______________________'
     O                                           94 '________________'
     /EJECT
    O*
    O* CA DECLARATION PAGE.
    O*
     O          e    44      dtmc           0 01
     O          e    44      nodtmc         0 01
     O          e    44      nodtmc         1
     O          e   n44      dtmc           0 02
     O          e   n44      nodtmc         0 03
     O          e   n44      formc          1
     O                                           28 'WORKERS COMPENSATION AND'
     O                                           52 ' EMPLOYERS LIABILITY INS'
     O                                           76 'URANCE POLICY           '
     O          e    47      formc          4
     O          e            formc          1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                           94 '__________________'
     O          e            formc          2
     O                                           21 'Insurer ID No(s).'
     O                       f2car#              27
     O                                           57 'INFORMATION PAGE'
     O               25                          82 'RENEWAL'
     O               24                          94 'REISSUE'
     O          e            formc          0
     O                                           94 '__________________'
     O          e            formc          1
     O                       fnname              64
     O                                           76 'File No.'
     O                       xpol                95
     O          e            formc          1
     O                       faa(1)              44
     O          e            formc          0
     O                                           94 '__________________'
     O          e            formc          1
     O                       faa(2)              44
     O                                           76 'Prior Policy No.'
     O                       xppol               95
     O          e            formc          1
     O                       faa(3)              44
     O          e            formc          0
     O                  14                       77 'X'
     O                  15                       91 'X'
     O          e            formc          1
     O                                           89 '_ Individual '
     O                                           97 '_ LLC  '
     O          e            formc          0
     O                  16                       77 'X'
     O                  17                       91 'X'
     O          e            formc          1
     O                                           22 '1.  Named Insured:'
     O                       xsnam1              75
     O                                           89 '_ Corporation'
     O                                           97 '_ LLP  '
     O          e            formc          0
     O                  18                       77 'X'
     O                  19                       91 'X'
     O          e            formc          1
     O                       xsnam2              75
     O                                           89 '_ Partnership'
     O                                           97 '_ Other'
     O          e            formc          1
     O               27      xsfdn1              75
     O          e            formc          1
     O               27      xsfdn2              75
     O          e            formc          1
     O                                           24 'Mailing Address:'
     O                       eaa(1)              65
     O          e            formc          1
     O                       eaa(2)              65
     O          e            formc          0
     O                                           94 '__________________'
     O          e            formc          1
     O                       eaa(3)              65
     O                                           76 'FEIN:'
     O                       xxfein              86
     O          e            formc          0
     O                                           94 '__________________'
     O          e            formc          1
     O                                           24 'Other workplaces'
     O                                           41 'not shown above:'
     O                                           64 'Intra/Interstate'
     O                                           76 'Risk ID No.'
     O                       xxRisk              92
     O          e            formc          2
     O               23N75                       33 'SEE SCHEDULE OF OPERATI'
     O               23N75                       36 'ONS'
     O               23 75                       33 'SEE SCHEDULE OF LOCATIO'
     O               23 75                       35 'NS'
     O              n23                          33 'NO OTHER WORKPLACES FOR '
     O              n23                          44 'THIS POLICY'
     O          e            formc          0
     O                       polPeriod1
     O          e            formc          1
     O                       polPeriodU
     O          e            formc          1
     O                       polPeriod2
     O          e            formc          2
     O                       polPeriod3
     O          e            formc          1
     O                                           28 '3.  A.  Workers Compensa'
     O                                           52 'tion Insurance: Part One'
     O                                           76 ' of the policy applies t'
     O                                           94 'o the Workers     '
     O          e            formc          1
     O                                           28 '        Compensation Law'
     O                                           52 ' of the states listed he'
     O                                           76 're:                     '
     O          e            formc          1
     O                       xst(1)              19
     O                       xst(2)              22
     O                       xst(3)              25
     O                       xst(4)              28
     O                       xst(5)              31
     O                       xst(6)              34
     O                       xst(7)              37
     O                       xst(8)              40
     O                       xst(9)              43
     O                       xst(10)             46
     O                       xst(11)             49
     O                       xst(12)             52
     O                       xst(13)             55
     O                       xst(14)             58
     O                       xst(15)             61
     O                       xst(16)             64
     O                       xst(17)             67
     O                       xst(18)             70
     O                       xst(19)             73
     O                       xst(20)             76
     O                       xst(21)             79
     O                       xst(22)             82
     O                       xst(23)             85
     O                       xst(24)             88
     O                       xst(25)             91
     O          e            formc          1
     O                       xst(26)             19
     O                       xst(27)             22
     O                       xst(28)             25
     O                       xst(29)             28
     O                       xst(30)             31
     O                       xst(31)             34
     O                       xst(32)             37
     O                       xst(33)             40
     O                       xst(34)             43
     O                       xst(35)             46
     O                       xst(36)             49
     O                       xst(37)             52
     O                       xst(38)             55
     O                       xst(39)             58
     O                       xst(40)             61
     O                       xst(41)             64
     O                       xst(42)             67
     O                       xst(43)             70
     O                       xst(44)             73
     O                       xst(45)             76
     O                       xst(46)             79
     O                       xst(47)             82
     O                       xst(48)             85
     O                       xst(49)             88
     O                       xst(50)             91
     O          e            formc          1
     O                                           28 '    B.  Employers Liabil'
     O                                           52 'ity Insurance: Part Two '
     O                                           76 'of the policy applies to'
     O                                           94 ' work in each     '
     O          e            formc          1
     O                                           28 '        state listed in '
     O                                           52 'Item 3A.  The limits of '
     O                                           76 'our liability under Part'
     O                                           93 ' Two are:        '
     O          e            formc          0
     O                                           41 'Bodily Injury by Acciden'
     O                                           46 't   $'
     O                       xxacc         j     61
     O                                           80 'each accident'
     O          e            formc          1
     O                                           66 '____________________'
     O          e            formc          0
     O                                           41 'Bodily Injury by Disease'
     O                                           46 '    $'
     O                       xxagg         j     61
     O                                           80 'policy limit '
     O          e            formc          1
     O                                           66 '____________________'
     O          e            formc          0
     O                                           41 'Bodily Injury by Disease'
     O                                           46 '    $'
     O                       xxdis         j     61
     O                                           80 'each employee'
     O          e            formc          2
     O                                           66 '____________________'
     O          e            formc          1
     O                                           32 'C.  Other States Insuran'
     O                                           35 'ce:'
     O                       xxpp3c              96
     O          e            formc          1
     O                       xxppc3              96
     O          e            formc          1
     O                                           28 '    D.  This policy incl'
     O                                           52 'udes these endorsements '
     O                                           76 'and schedules:          '
     O          e    95      formc          2
     O                                           35 'SEE SCHEDULE OF FORMS'
     O          e            formc          1
     O               58                          28 '4.  The premium for this'
     O              N58                          28 '4.  The Cntrbtn for this'
     O                                           52 ' policy will be determin'
     O                                           76 'ed by our Manuals of Rul'
     O                                           94 'es,               '
     O          e            formc          1
     O                                           28 '    Classifications, Rat'
     O                                           52 'es and Rating Plans.  Al'
     O                                           76 'l information required b'
     O                                           94 'elow is subject   '
     O          e            formc          2
     O                                           28 '    to verification and '
     O                                           52 'change by audit.        '
     O          e            formc          1
     O                                           12 'Code'
     O                                           37 'Classifications'
     O               58                          59 'Premium Basis'
     O              N58                          59 'Cntrbtn Basis'
     O                                           77 'Rate Per'
     O                                           94 'Estimated'
     O          e            formc          1
     O                                           12 'No.'
     O                                           61 'Estimated Annual'
     O                                           77 '$100 of'
     O                                           93 'Annual'
     O          e            formc          1
     O                                           58 'Remuneration'
     O                                           79 'Remuneration'
     O               58                          93 'Premium'
     O              N58                          93 'Cntrbtn'
     O          e            formc          2
     O                                           37 'SEE SCHEDULE OF OPERATI'
     O                                           40 'ONS'
     O          e            formc          0
     O                                           47 '_______________'
     O                                           97 '______________'
     O          e            formc          2
     O                                           31 'Experience Modification'
     O                       xxexm1        2     40
     O                                           73 'Total Estimated Annual '
     O               58                          82 'Premium $'
     O              N58                          82 'Cntrbtn $'
     O                       xxprm         1     97
     O          e            formc          0
     O                                           47 '_____________________'
     O               46                          64 '_________________'
     O                                           97 '______________'
     O          e            formc          2
     O              n46 58                       25 'Minimum Premium $'
     O              n46N58                       25 'Minimum Cntrbtn $'
     O              n46      xxmnpr        1     40
     O               46                          26 'Agent Information:'
     O               46      xxprag              63
     O               58                          82 'Deposit Premium $'
     O              N58                          82 'Deposit Cntrbtn $'
     O                       xxpdpr        1     97
     O          e            formc          0
     O                                           47 '____________'
     O               46                          97 '______________'
     O          e            formc          2
     O               58                          27 'Premium Adjustment '
     O              N58                          27 'Cntrbtn Adjustment '
     O                                           34 'Period:'
     O                                           44 'Annually'
     O              n46                          66 'Countersigned by'
     O              n46                          90 '_______________________'
     O              n46                          97 '_______'
     O               46 58                       82 'Minimum Premium $'
     O               46N58                       82 'Minimum Cntrbtn $'
     O               46      xxmnpr        1     97
     O          e   n46      formc          0
     O                                           51 '________________________'
     O                                           75 '________________________'
     O                                           97 '______________________'
     O          e   n46      formc          2
     O                                           26 'Agent Information:'
     O                       xxprag              63
     O          e            formc          0
     O                                           57 '________________________'
     O                                           75 '__________________'
     O                                           97 '______________'
     O          e            formc          2
     O                                           32 'Servicing/Issuing Office'
     O                                           82 'Date'
     O                       xxofad              74
     O                       xxdate              95 '  /  /    '
     O          e   n46      formc          1
     O                                           17 'WC 04 00 01 A'
     O                                           40 '@1998 by the Workers'''
     O                                           63 'Compensation Insurance'
     O                                           86 'Rating Bureau of Calif'
     O                                           96 'ornia. All'
     O          e   n46      formc          1
     O                                           26 'rights reserved. From '
     O                                           48 'the WCIRB''s CA Workers'
     O                                           70 ''' Compensation Insuran'
     O                                           92 'ce Forms Manual @2001.'
     /EJECT
    O*
    O* PA DECLARATION PAGE FOR GROUP 2 - Client 114
    O*
     O          e    44      dtm2           0 06
     O          e    44      nodtm2         0 02
     O          e    44      nodtm2         1
     O          e   n44      dtm2           0 04
     O          e   n44      nodtm2         0 05
     O          e            FORMP       2  0
     O                                           28 'PENNSYLVANIA PUBLIC LAW '
     O                                           52 '477 LIABILITY INSURANCE '
     O                                           76 'POLICY                  '
     O          e            FORMP          3
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '______                  '
     O          e            FORMP          1
     O                                           12 'Insurer:'
     O                                           81 'Producer: Agent#'
     O                       shagt#        z     91
     O          e            FORMP          1
     O                       fnname              64
     O                       xxagnm              99
     O          e            FORMP          1
     O                       fnmad1              44
     O                       xxagad              99
     O          e    20      FORMP          1
     O                       fnmad2              44
     O                       xxacsz              99
     O          e            FORMP          2
     O                       xxfcsz              44
     O              n20      xxacsz              99
     O          e            FORMP          2
     O                                           76 'File#:   '
     O                       xpol                95
     O          e            FORMP          1
     O                                           16 'The Insured:'
     O                       x1name              83
     O          e            FORMP          1
     O                       xxadh2              20
     O                       x2name              83
     O          e            FORMP          1
     O                       xxadh3              20
     O                       xxemn3              76
     O          e            FORMP          1
     O                       xxadh4              20
     O                       xxemn4              76
     O          e            FORMP          1
     O                       xxadh5              20
     O                       xxemn5              76
     O          e            FORMP          1
     O                       xxemn6              76
     o                                          102 '.'
     O          e            FORMP          2
     O                       xxemn7              66
     O          e            FORMP          0
     O                                           38 'PENNSYLVANIA PUBLIC LAW '
     O                                           62 '477 LIABILITY INSURANCE '
     O                                           86 'COVERAGE PART DECLARATIO'
     O                                           88 'NS'
     O          e            FORMP          2
     O                                           38 '________________________'
     O                                           62 '________________________'
     O                                           86 '________________________'
     O                                           88 '__'
     O          e            FORMP          0
     O                                           28 '1.      The policy perio'
     O                                           51 'd is from 12:01a.m. on '
     O                       xxpfdt        y     62
     O                                           79 'to 12:01a.m. on '
     O                       xxptdt        y     90
     O          e            FORMP          1
     O                                           63 '_____________'
     O                                           91 '_____________'
     O          e            FORMP          2
     O                                           35 'at the insured''s mailin'
     O                                           45 'g address.'
     O          e            FORMP          2
     O                                           28 '2.      Audit period is '
     O                                           52 'annual, unless otherwise'
     O                                           76 ' stated.                '
     O          e            FORMP          1
     O                                           28 '3.      Limits of Insura'
     O                                           52 'nce                     '
     O          e            FORMP          1
     O                                           32 '$'
     O                       xxdis         j     44
     O                                           57 'Per Person   '
     O          e            FORMP          2
     O                                           32 '$'
     O                       xxacc         j     44
     O                                           57 'Per Accident '
     O          e            FORMP          1
     O                                           28 '4.      Named Insured is'
     O                                           29 ':'
     O          e            FORMP          1
     O                                           13 '('
     O                       x1                  17
     O                                           21 ')'
     O                                           35 'Individual   '
     O                                           41 '('
     O                       x2                  45
     O                                           49 ')'
     O                                           63 'Joint Venture'
     O                                           72 '('
     O                       x3                  76
     O                                           80 ')'
     O                                           94 'Partnership  '
     O          e            FORMP          1
     O                                           13 '('
     O                       x4                  17
     O                                           21 ')'
     O                                           35 'Corporation  '
     O          e            FORMP          2
     O                                           13 '('
     O                       x5                  17
     O                                           21 ')'
     O                                           35 'Other        '
     O          e            FORMP          1
     O                                           33 'Business Description:'
     O          e            FORMP          2
     O                       xxbdes              55
     O          e            FORMP          2
     O                                           28 '5.      Forms and Endors'
     O                                           52 'ements Applicable at Tim'
     O                                           76 'e of Issuance:          '
     O          e            FORMP          3
     O                                           44 'See Attached Schedule of'
     O                                           68 ' Forms and Endorsements '
     O          e            FORMP          2
     o                                          102 '.'
     O          e            FORMP          1
     O               58                          28 '6.      Premium For This'
     O              N58                          28 '6.      Cntrbtn For This'
     O                                           52 ' Coverage Part:         '
     O               58                          76 'Premium Payable at Incep'
     O              N58                          76 'Cntrbtn Payable at Incep'
     O                                           94 'tion:             '
     O          e            FORMP          2
     O                       xxprm         1     35 '$'
     O                       xxpdpr        1     73 '$'
     O          e            FORMP          1
     O                                           28 '7.      These Declaratio'
     O                                           52 'ns and the General Decla'
     O                                           76 'rations, If Applicable, '
     O                                          100 'Together With The       '
     O          e            FORMP          1
     O                                           28 '        Common Policy Co'
     O                                           52 'nditions, Coverage Form('
     O                                           76 's) and Forms and Endorse'
     O                                          100 'ments, If Any,          '
     O          e            FORMP          2
     O                                           28 '        Issued To Form A'
     O                                           52 ' Part Thereof, Complete '
     O                                           76 'The Above Numbered Polic'
     O                                          100 'y.                      '
     O          e            FORMP          1
     o                                          102 '.'
     O          e            FORMP          1
     O              n46                          31 'Countersigned By'
     O              n46                          56 '_________________________'
     O              n46                          71 'Date'
     O              n46                          92 '_____________________'
     O          e   n46      FORMP          2
     O              n46                          51 'Authorized Agent'
     O          e   n46      FORMP          1
     O              n46                          11 'GL-HL-1'
     /EJECT
     Oqsysprt1  e    95n75   sched          1 04
     O                                           27 'SCHEDULE OF ENDORSEMENT'
     O                                           33 'S FOR:'
     O          e    95n75   sched          1
     O                       xxenam              66
     O          e    95n75   sched          1
     O                       xxead1              66
     O          e    95n75 21sched          1
     O                       xxead2              66
     O          e    95n75   sched          1
     O                       xxecsz              66
     O          e    95n75   sched          1
     O              n80n81                       18 'POLICY NUMBER:'
     O               80n81                       26 'CARRIER POLICY NUMBER:'
     O                  81                       18 'POLICY NUMBER:'
     O          e    95n75   sched          1
     O                       xpol                22
     O          e    95n75   sched       1  1
     O                       xf1(1)              39
     O                       xf1(2)              77
     O          e    95n75   sched       1  1
     O                       xf1(3)              39
     O                       xf1(4)              77
     O          e    95n75   sched       1  1
     O                       xf1(5)              39
     O                       xf1(6)              77
     O          e    95n75   sched       1  1
     O                       xf1(7)              39
     O                       xf1(8)              77
     O          e    95n75   sched       1  1
     O                       xf1(9)              39
     O                       xf1(10)             77
     O          e    95n75   sched       1  1
     O                       xf1(11)             39
     O                       xf1(12)             77
     O          e    95n75   sched       1  1
     O                       xf1(13)             39
     O                       xf1(14)             77
     O          e    95n75   sched       1  1
     O                       xf1(15)             39
     O                       xf1(16)             77
     O          e    95n75   sched       1  1
     O                       xf1(17)             39
     O                       xf1(18)             77
     O          e    95n75   sched       1  1
     O                       xf1(19)             39
     O                       xf1(20)             77
     O          e    95n75   sched       1  1
     O                       xf1(21)             39
     O                       xf1(22)             77
     O          e    95n75   sched       1  1
     O                       xf1(23)             39
     O                       xf1(24)             77
     O          e    95n75   sched       1  1
     O                       xf1(25)             39
     O                       xf1(26)             77
     O          e    95n75   sched       1  1
     O                       xf1(27)             39
     O                       xf1(28)             77
     O          e    95n75   sched       1  1
     O                       xf1(29)             39
     O                       xf1(30)             77
     O          e    95n75   sched       1  1
     O                       xf1(31)             39
     O                       xf1(32)             77
     O          e    95n75   sched       1  1
     O                       xf1(33)             39
     O                       xf1(34)             77
     O          e    95n75   sched       1  1
     O                       xf1(35)             39
     O                       xf1(36)             77
     O          e    95n75   sched       1  1
     O                       xf1(37)             39
     O                       xf1(38)             77
     O          e    95n75   sched       1  1
     O                       xf1(39)             39
     O                       xf1(40)             77
     O          e    95n75   sched       1  1
     O                       xf1(41)             39
     O                       xf1(42)             77
     O          e    95n75   sched       1  1
     O                       xf1(43)             39
     O                       xf1(44)             77
     O          e    95n75   sched       1  1
     O                       xf1(45)             39
     O                       xf1(46)             77
     O          e    95n75   sched       1  1
     O                       xf1(47)             39
     O                       xf1(48)             77
     O          e    95n75   sched       1  1
     O                       xf1(49)             39
     O                       xf1(50)             77
     O          e    95n75   sched            62
     O                       dtmdot               1
     /EJECT
    O*
    O* California Version
    O*
     Oqsysprt1  e    95 75   nodtms         0 03
     Oqsysprt1  e    95 75   dtms           0 04
     O          e    95 75   sched          1
     O                                           28 'WORKERS COMPENSATION AND'
     O                                           52 ' EMPLOYERS LIABILITY INS'
     O                                           76 'URANCE POLICY           '
     O          e    95 75   sched          2
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                           79 '___'
     O          e    95 75   sched          2
     O                                           47 'EXTENSION OF '
     O                                           63 'INFORMATION PAGE'
     O          e    95 75   sched          2
     O                                           58 'Schedule of Forms'
     O          e    95 75   sched          2
     O                                           53 'ITEM 3D'
     O          e    95 75   sched          0
     O                                           77 '__________________'
     O          e    95 75   sched          2
     O                                           58 'FILE NO.'
     O                       xpol                78
     O          e    95 75   sched          0
     O                                           16 '____________'
     O                                           39 '_________________'
     O                                           56 '____________'
     O                                           79 '_________________'
     O          e    95 75   sched          2
     O                                           16 'Form Numbers'
     O                                           39 'Applicable States'
     O                                           56 'Form Numbers'
     O                                           79 'Applicable States'
     O          e    95 75   sched          1
     O                       xf(1)               20
     O                       xf(2)               60
     O          e    95 75   sched          1
     O                       xf(3)               20
     O                       xf(4)               60
     O          e    95 75   sched          1
     O                       xf(5)               20
     O                       xf(6)               60
     O          e    95 75   sched          1
     O                       xf(7)               20
     O                       xf(8)               60
     O          e    95 75   sched          1
     O                       xf(9)               20
     O                       xf(10)              60
     O          e    95 75   sched          1
     O                       xf(11)              20
     O                       xf(12)              60
     O          e    95 75   sched          1
     O                       xf(13)              20
     O                       xf(14)              60
     O          e    95 75   sched          1
     O                       xf(15)              20
     O                       xf(16)              60
     O          e    95 75   sched          1
     O                       xf(17)              20
     O                       xf(18)              60
     O          e    95 75   sched          1
     O                       xf(19)              20
     O                       xf(20)              60
     O          e    95 75   sched          1
     O                       xf(21)              20
     O                       xf(22)              60
     O          e    95 75   sched          1
     O                       xf(23)              20
     O                       xf(24)              60
     O          e    95 75   sched          1
     O                       xf(25)              20
     O                       xf(26)              60
     O          e    95 75   sched          1
     O                       xf(27)              20
     O                       xf(28)              60
     O          e    95 75   sched          1
     O                       xf(29)              20
     O                       xf(30)              60
     O          e    95 75   sched          1
     O                       xf(31)              20
     O                       xf(32)              60
     O          e    95 75   sched          1
     O                       xf(33)              20
     O                       xf(34)              60
     O          e    95 75   sched          1
     O                       xf(35)              20
     O                       xf(36)              60
     O          e    95 75   sched          1
     O                       xf(37)              20
     O                       xf(38)              60
     O          e    95 75   sched          1
     O                       xf(39)              20
     O                       xf(40)              60
     O          e    95 75   sched          1
     O                       xf(41)              20
     O                       xf(42)              60
     O          e    95 75   sched          1
     O                       xf(43)              20
     O                       xf(44)              60
     O          e    95 75   sched          1
     O                       xf(45)              20
     O                       xf(46)              60
     O          e    95 75   sched          1
     O                       xf(47)              20
     O                       xf(48)              60
     O          e    95 75   sched          1
     O                       xf(49)              20
     O                       xf(50)              60
     O          e    95 75   sched          1 59
     O                                           16 'WC 04 00 04 '
     O                                           38 '@1998 by the Workers'''
     O                                           61 'Compensation Insurance'
     O                                           71 'Bureau of'
     O          e    95 75   sched          1
     O                                           26 'California. All rights'
     O                                           49 'reserved. From the WCI'
     O                                           70 'RB''s California Worke'
     O                                           73 'rs'''
     O          e    95 75   sched          1
     O                                           26 'Compensation Insurance'
     O                                           46 'Forms Manual @2001.'
     /EJECT
    O*
    O* Group 2 PA Schedule of Forms for Client 114
    O*
     Oqsysprt1  e            nodtmsp        0 03
     Oqsysprt1  e            dtmsp          0 04
     O          e            SCHDP          1
     O                                           12 'Insurer:'
     O                                           67 'Producer: Agent#'
     O                       shagt#        z     77
     O          e            SCHDP          1
     O                       fnname              64
     O                       xxagnm              85
     O          e            SCHDP          1
     O                       fnmad1              44
     O                       xxagad              85
     O          e    20      SCHDP          1
     O                       fnmad2              44
     O                       xxacsz              85
     O          e            SCHDP          2
     O                       xxfcsz              44
     O              n20      xxacsz              85
     O          e            SCHDP          2
     O                                           62 'File#:   '
     O                       xpol                81
     O          e            SCHDP          1
     O                                           16 'The Insured:'
     O                       x1name              79
     O          e            SCHDP          1
     O                       xxadh2              20
     O                       x2name              79
     O          e            SCHDP          1
     O                       xxadh3              20
     O                       xxemn3              72
     O          e            SCHDP          1
     O                       xxadh4              20
     O                       xxemn4              72
     O          e            SCHDP          1
     O                       xxadh5              20
     O                       xxemn5              72
     O          e            SCHDP          1
     O                       xxemn6              72
     o                                           85 '.'
     O          e            SCHDP          2
     O                       xxemn7              62
     O          e            SCHDP          0
     O                                           48 'Policy Forms and Endorse'
     O                                           72 'ments Schedule          '
     O          e            SCHDP          2
     O                                           48 '________________________'
     O                                           72 '______________          '

     O          e            SCHDP          0
     O                                           15 'Form Number'
     O                                           61 'Form Title'
     O          e            SCHDP          2
     O                                           15 '___________'
     O                                           61 '__________'
     O          e            SCHDD          1
     O                       x1form              39
     O                       x1desc              81
      *
     O          e            SCHDL          1 59
     O                                           11 'GL-HL-1'
     /EJECT
     Oqsysprt1  e      n75   sohead         2 05
     O              n32N81                       30 'SCHEDULE OF OPERATIONS'
      *
     O               32N81                       25 'EXTENSION OF INFORMATI'
     O               32N81                       32 'ON PAGE'
     O                 N81                       37 'FOR:'
     O                  81                       30 'Extension of Informati'
     O                  81                       37 'on Page'
     O                  81   xxetxt            +001
     O                                           75 'PAGE:'
     O                       xxpage        z     79
     O          e    94n75   sohead         2
     O                       xxCtr             +020
     O                                         +000 'MATION FOR '
     O                       xxpst             +000
     O                                         +000 '  *****'
     O          e      n75   sohead         1
     O                       xxenm3              40
     O              n81                          60 'Carrier Policy #:'
     O               81                          60 'Policy #:'
     O                       xpol                79
     O          e    27n75   sohead         1
     O                       xxenm2              40
     O                                           60 'Fein:'
     O                       xxfein              70
     O          e      n75   sohead         1
     O                       xxead1              45
     O              n27                          60 'Fein:'
     O              n27      xxfein              70
     O          e    21n75   sohead         1
     O                       xxead2              45
     O          e    82n75   sohead         1
     O                       xxecsz              45
     O                       xxecsz              45
     O          e   n82n75   sohead         2
     O                       xxecsz              45
     O                       x4txid         b    80
     O          e    82n75   sohead         2
     O                                           49 'DIV #:'
     O                       xxhdiv              55
     O                                           67 'E/L Number:'
     O                       xxhlid              78
    O*
    O* California Version
    O*
     Oqsysprt1  e       75   sohead         1 03
     O                                           28 'WORKERS COMPENSATION AND'
     O                                           52 ' EMPLOYERS LIABILITY INS'
     O                                           76 'URANCE POLICY           '
     O          e       75   sohead         2
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                           77 '_'
     O          e       75   sohead         2
     O                                           39 'EXTENSION OF '
     O                                           55 'INFORMATION PAGE'
     *
     /EJECT
     O          e      n75   addnmh         2
     O                                           20 'ADDITIONAL NAMED'
     O                                         +  1 'INSUREDS'
    O*
    O* California Version
    O*
     O          e       75   addnmh         2
     O                                           45 'Schedule of Named'
     O                                           53 ' Insured'
     O          e       75   addnmh         2
     O                                           43 'ITEM 1'
     O          e       75   addnmh         0
     O                                           77 '__________________'
     O          e       75   addnmh         2
     O                                           58 'FILE NO.'
     O                       xpol                78
     O          e       75   addnmh         0
     O                                           17 '_____________'
     O                                           65 '____'
     O          e       75   addnmh         2
     O                                           17 'Named Insured'
     O                                           65 'FEIN'
     *
     O          e    76      addnam         1
     O                       ##PEO1              62
     O          e    77      addnam         1
     O                       ##PEO2              62
     O          e            addnam         1
     O                       anname              42
     O                 n75                       60 'Fein:'
     O                 n75   anfein            +  1
     O                  75   anfein              70
     O          e            addnam         1
     O               69                          13 'Eff date:'
     O               69      XOEFDT              24 '  /  /    '
     O               79                          40 'Exp date:'
     O               79      XODLDT              51 '  /  /    '
     O          e            addnm2         1
     O                       anname              42
     O                 n75                       60 'Fein:'
     O                 n75   anfein            +  1
     O                  75   anfein              70
     O          e            addnm2         1
     O                       annam2              42
     O          e            addnm2         1
     O               69                          13 'Eff date:'
     O               69      XOEFDT              24 '  /  /    '
     O               79                          40 'Exp date:'
     O               79      XODLDT              51 '  /  /    '
     O          e            dtma           0 57
     O          e            nodtma         0 59
     O          e      n75   addlst         1
     O                       frmNmA              17
     O          e       75   addlst         1
     O                                           16 'WC 04 00 02 '
     O                                           38 '@1998 by the Workers'''
     O                                           61 'Compensation Insurance'
     O                                           71 'Bureau of'
     O          e       75   addlst         1
     O                                           26 'California. All rights'
     O                                           49 'reserved. From the WCI'
     O                                           70 'RB''s California Worke'
     O                                           73 'rs'''
     O          e       75   addlst         1
     O                                           26 'Compensation Insurance'
     O                                           46 'Forms Manual @2001.'
     *
     /EJECT
     O          e      n75   othphd         2
     O                                           22 'OTHER WORKPLACES:'
    O*
    O* California Version
    O*
     O          e       75   othphd         2
     O                                           51 'Schedule of Locations'
     O          e       75   othphd         2
     O                                           43 'ITEM 1'
     O          e       75   othphd         0
     O                                           77 '__________________'
     O          e       75   othphd         2
     O                                           58 'FILE NO.'
     O                       xpol                78
     O          e            othnam         1
     O                       xxnm56              61
     O               29      XOFEIH              66
     O               29      XOFEID              76
     O          e            othpdt1        1
     O                       xo1Ln               80
     O          e            dtmo           0 57
     O          e            nodtmo         0 59
     O          e      n75   othlst         1
     O                       frmNmO              17
     O          e       75   othlst         1
     O                                           16 'WC 04 00 03 '
     O                                           38 '@1998 by the Workers'''
     O                                           61 'Compensation Insurance'
     O                                           71 'Bureau of'
     O          e       75   othlst         1
     O                                           26 'California. All rights'
     O                                           49 'reserved. From the WCI'
     O                                           70 'RB''s California Worke'
     O                                           73 'rs'''
     O          e       75   othlst         1
     O                                           26 'Compensation Insurance'
     O                                           46 'Forms Manual @2001.'
     /EJECT
     O          e      n75   payrhd      1  2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          e      n75   payrhd         1
     O                 N81                       29 'Code    Classification  '
     O              n84N81                       65 'Payroll       Rate'
     O               84N81                       65 'Payroll   Net Rate'
     O               58N81                       79 'Premium'
     O              N58N81                       79 'Cntrbtn'
     o*
     O                  81                       17 'Classifications'
     O                  81                       39 'Code'
     O               58 81                       54 'Premium Basis'
     O              N58 81                       54 'Cntrbtn Basis'
     O                  81                       66 'Rate Per'
     O                  81                       79 'Estimated'
     O          e   n75 81   payrhd         1
     O                                           39 'No.'
     O                                           56 'Total Est Annual'
     O                                           66 '$100 of'
     O                                           78 'Annual'
     O          e   n75 81   payrhd         1
     O                                           53 'Remuneration'
     O                                           68 'Remuneration'
     O               58                          78 'Premium'
     O              N58                          78 'Cntrbtn'
     O          e      n75   payrhd         2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
    O*
    O* California Version
    O*
     O          e       75   payrhd         2
     O                                           48 'Classifications'
     O          e       75   payrhd         2
     O                                           43 'ITEM 4'
     O          e    94 75   payrhd         2
     O                       xxCtr             +020
     O                                         +000 'MATION FOR '
     O                       xxpst             +000
     O                                         +000 '  *****'
     O          e       75   payrhd         0
     O                                           77 '__________________'
     O          e       75   payrhd         2
     O                                           58 'FILE NO.'
     O                       xpol                78
     O          e       75   payrhd         1
     O                                            7 'Code'
     O                                           28 'Classifications'
     O               58                          54 'Premium Basis'
     O              N58                          54 'Cntrbtn Basis'
     O                                           66 'Rate Per'
     O                                           79 'Estimated'
     O          e       75   payrhd         1
     O                                            7 'No.'
     O                                           56 'Estimated Annual'
     O                                           66 '$100 of'
     O                                           78 'Annual'
     O          e       75   payrhd         1
     O                                           53 'Remuneration'
     O                                           68 'Remuneration'
     O               58                          78 'Premium'
     O              N58                          78 'Cntrbtn'
     O          e       75   payrhd         2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          e            payrdt         1
     O                  75   X3clas#              7
     O               78 75   epspec               9
     O               78 75   xxext               15
     O              N81n75   X3clas        z      9
     O              N81n75   epspec              11
     O              N81n75   xxext               17
     O               81      x@desc              32
     O                       x2eapr        2     54
     O              N81      x@desc              38
     O               81n75   X3clas        z     39
     O               81n75   epspec              41
     O              n84      eparat        2     65
     O               84      xnarat        2     65
     O              n84      epempr        1     79
     O               84      xnempr        1     79
     O                       xnemsn              80
     O          e            payrdt         1
     O               91                          14 '('
     O               91      xxfrav        y     22
     O               91                          23 '-'
     O               91      xxtoav        y     31
     O               91                          32 ')'
     O               92      epadld        4     52
     O               92n64                       44 'Add''l Load'
     O               92 64 91                    44 'Terr. Diff.'
     O               92 64n91                    44 'Territory Diff.'
     O               92 35                       44 '     Charge'
     O               83                        +002 'TDF:'
     O               83      x3TDF             +002 '0 .    '
     O          e    93      payrdt         1
     O                                           27 '93 Rate Credit'
     O                       epdisc        j     80
     O          e            payrnd         1
     O                                           32 'NO PAYROLL CATEGORIES WE'
     O                                           56 'RE REPORTED FOR THIS EMP'
     O                                           62 'LOYER.'
     O          e            paylin      1  2
     O                                           25 '________________________'
     O                                           48 '________________________'
     O                                           71 '________________________'
     O                                           80 '_________'
     O          e            prmdet         1
     O                       xxhdg               55
     O                       xxtam         2     79
     O          e            prmexp      1  1
     O              n96 98                       25 'Experience Modifiers'
     O               96 98                       25 'Merit Rating        '
     O                       xxTerHdg            74
     O          e            prmexp         1
     O                  98   xxexm1        2      9
     O                  98   xxemd1              20 '  /  /    '
     O                       xxTerFac      2     54
     O          e            prmexp         1
     O                  98   xxexm2        2      9
     O                  98   xxemd2              20 '  /  /    '
     O          e            prmexp         2
     O                  98   xxexm3        2      9
     O                  98   xxemd3              20 '  /  /    '
     O          e            premFooter       60
     O                       footE               23
     O          e            lstlin           62
     O                       dtmdot               1
     O          e            dtmp           0 57
     O          e            nodtmp         0 59
     O          e            paylst         1
     O                       frmNmP              17
     O               75                          38 '@1998 by the Workers'''
     O               75                          61 'Compensation Insurance'
     O               75                          71 'Bureau of'
     O          e    75      paylst         1
     O                                           26 'California. All rights'
     O                                           49 'reserved. From the WCI'
     O                                           70 'RB''s California Worke'
     O                                           73 'rs'''
     O          e    75      paylst         1
     O                                           26 'Compensation Insurance'
     O                                           46 'Forms Manual @2001.'
