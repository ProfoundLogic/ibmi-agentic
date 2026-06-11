      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  06/24/14 C:053 Support: 86118                                *
      *                 If client 129 print heading further down      *
      *                                                               *
      *  06/11/12 C:277 PROJECT 114MER                                *
      *                 Convert from RPG to RPGLE                     *
      *                                                               *
      * 04/27/07 C:15  Project: 71WHI                                 *
      *                71WCO: Removed "Paid by" for clt 71.           *
      *
      * 11/28/05 C:53  Project: 136WAH
      *                MOVE BANK NAME ABOVE THE CHECK #
      *
      * 08/04/05 C:53  PROJECT: 132WCN                                *
      *                Correct wording in fraud stmt.                 *
      *
      * 04/25/05 C:156 PROJECT: 132WCN                                *
      *                Print CA fraud statement on checks.            *
      *                                                               *
      * 04/12/04 C:62  Project: WC1220
      *                Change where prints when group check
      *
      * 10/20/03 C:53  Project: WC909
      *                Added country info.
      *
      * ******************************************************************
      *
      *  FILECOPY CHECK STUB
      *
     O*** Beg Del *** 06/24/14 ****************************************
     O***FILECOPY  E   N17      DET2           1 25
     O*** End Del *** 06/24/14 ****************************************
     O*** Beg Add *** 06/24/14 ****************************************
     OFILECOPY  E   N17N71   DET2           1 25
     OFILECOPY  E   N17 71   DET2           1 32
     O*** End Add *** 06/24/14 ****************************************
     O                       XXTXT1              68
     OFILECOPY  E    17      DET2        2  1  3
     O                       XXTXT1              68
     O          E            DET2           1
     O                       XXFN               112
     O          E            DET2           1
     O                       XXFAD1              92
     O          E            DET2           1
     O              N65      XXFAD2              92
     O               65      XXFCT               77
     O               65      XXFST               80
     O               65      XXFZIP              93
     O          E            DET2           1
     O              N65      XXFCT               77
     O              N65      XXFST               80
     O              N65      XXFZIP              93
     O          E            DET2           1
     O                                           12 'PAYEE...:'
     O                       @@PNAM              53
     O          E            DET2           1
     O                       @@ADR1              53
     O          E            DET2           1
     O              N61      @@ADR2              53
     O               61      XXCSZ               53
     O          E            DET2           1
     O              N61      XXCSZ               53
     O               61      XXADR4              53
     O              N61      XXADR4              95
     O          E            DET2           1
     O                                           12 'TAX ID#.:'
     O              N44      @@SS#               24
     O               44      @@FEIN              22
     O               44      @@MOD         Z     26
     O          E            DET2           1
     O                                           12 'EMPLOYER:'
     O                       EMEMP#        Z     22
     O                       EMDIV         Z     28
     O                       EMNAME              69
     O          E            DET2           1
     O                                           12 'CLAIM #.:'
     O                       @@CO#               16
     O                                           17 '-'
     O                       @@FND               20
     O                                           21 '-'
     O                       @@FYR               24
     O                                           25 '-'
     O                       @@CASE              32
     O                                           52 'CLAIMANT:'
     O                       @@NAME             100
     O          E            DET2           1
     O                                           12 'D/A.....:'
     O                       @@ACDT        Y     21
     O                       X1ADJ               62
     O                                           63 ':'
     O                       XXCLEX              67
     O                       XXUSNM             108
     O          E            DET2           1
     O                                           30 '- - - - - - - - - - '
     O                                           50 '- - - - - - - - - - '
     O                                           70 '- - - - - - - - - - '
     O                                           90 '- - - - - - - - - - '
     O                                          110 '- - - - - - - - - - '
     O                                          120 '- - - - - '
     O          E            DET2           1
     O                                           18 'CHECK#........:'
     O                       @@CHK#        3     27
     O                                           68 'PMT VAL 1.....:'
     O                       @@BR2               71
     O                       XXB2DS             102
     O          E            DET2           1
     O                                           18 'CHECK AMOUNT..:'
     O                       @@AMT               33 '   ,   ,  0.  '
     O                                           68 'RESERVE.......:'
     O                       @@RECD              71
     O                       XXRTD2             102
     O          E            DET2           1
     O                                           18 'CHECK DATE....:'
     O                       @@DATE        Y     27
     O                                           68 'PAYMENT PERIOD:'
     O                       XXFRDT        Y     77
     O                                           79 '-'
     O                       XXTODT        Y     88
     O              N66                         102 'WEEKS/DAYS:'
     O              N66      @@WKS         3    107
     O                                          109 '/'
     O              N66      @@DAYS             111
     O          E            DET2           1
     O                                           30 '- - - - - - - - - - '
     O                                           50 '- - - - - - - - - - '
     O                                           70 '- - - - - - - - - - '
     O                                           90 '- - - - - - - - - - '
     O                                          110 '- - - - - - - - - - '
     O                                          120 '- - - - - '
     O          E            DET2           1
     O                                           68 'REMARKS'
     O          E            DET2           1
     O                       XXREM1              43
     O                       XXREM2              84
     O          E            DET2           1
     O                       XXREM3              43
     O                       XXREM4              84
     O          E            DET2           1
     O                       RPRMDS              33
     O          E            DET2           1
     O                                           30 '- - - - - - - - - - '
     O                                           50 '- - - - - - - - - - '
     O                                           70 '- - - - - - - - - - '
     O                                           90 '- - - - - - - - - - '
     O                                          110 '- - - - - - - - - - '
     O                                          120 '- - - - - '
     O          E            FRAUD       1  1
     O                                           26 'INS. CODE SEC. 1871.8. A'
     O                                           50 'N INSURER OR SELF-INSURE'
     O                                           74 'D EMPLOYER SHALL PROVIDE'
     O                                           98 ' THE FOLLOWING NOTICE, I'
     O                                          112 'N BOTH ENGLISH'
     O          E            FRAUD          2
     O                                           26 'AND SPANISH, TO AN INJUR'
     O                                           50 'ED WORKER ON OR WITH A C'
     O                                           74 'HECK FOR TEMPORARY DISAB'
     O                                           89 'ILITY BENEFITS:'
     O          E            FRAUD          1
     O                                           26 'WARNING:  YOU ARE REQUIR'
     O                                           50 'ED TO REPORT TO YOUR EMP'
     O                                           74 'LOYER OR THE INSURANCE C'
     O                                           98 'OMPANY ANY MONEY THAT YO'
     O                                          115 'U EARNED FOR WORK'
     O          E            FRAUD          1
     O                                           26 'DURING THE TIME COVERED '
     O                                           50 'BY THIS CHECK, AND BEFOR'
     O                                           74 'E CASHING THIS CHECK.  I'
     O                                           98 'F YOU DO NOT FOLLOW THES'
     O                                          120 'E RULES, YOU MAY BE IN'
     O          E            FRAUD          2
     O                                           26 'VIOLATION OF THE LAW AND'
     O                                           50 ' THE PENALTY MAY BE JAIL'
     O                                           74 ' OR PRISON, A FINE, AND '
     O                                           91 'LOSS OF BENEFITS.'
     O          E            FRAUD          1
     O                                           26 'ADVERTENCIA:  ES NECESAR'
     O                                           50 'IO QUE USTED LE AVISE A '
     O                                           74 'SU PATRON O A SU COMPANI'
     O                                           98 'A DE SEGURO TODO DINERO '
     O                                          117 'QUE USTED HA GANADO'
     O          E            FRAUD          1
     O                                           26 'POR TRABAJAR, DURANTE EL'
     O                                           50 ' TIEMPO CUBIERTO POR EST'
     O                                           74 'E CHEQUE, Y ANTES DE CAM'
     O                                           98 'BIAR ESTE CHEQUE.  SI US'
     O                                          116 'TED NO SIGUE ESTOS'
     O          E            FRAUD          1
     O                                           26 'REGLAMENTOS, USTED PUEDE'
     O                                           50 ' ESTAR EN VIOLACION DE L'
     O                                           74 'A LEY Y EL CASTIGO PODRI'
     O                                           98 'A SER CARCEL O PRISION, '
     O                                          118 'UNA MULTA, Y PERDIDA'
     O          E            FRAUD          1
     O                                           16 'DE BENEFICIOS.'
