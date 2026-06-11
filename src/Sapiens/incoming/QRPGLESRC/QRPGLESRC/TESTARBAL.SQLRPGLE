     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H BNDDIR('WTBNDDIR')
       // Test ARBalance procedure in WPIar.
       // Put TestArBal in debug and make the
       // following call:
       //  SBMJOB CMD(
       //  CALL PGM(TESTARBAL)
       //  PARM(
       //    X'0000000F'
       //    X'1211201F'
       //    X'777F'
       //    X'900F'
       //    X'121F'
       //    X'000001484F'
       //    X'00000F'
       //    'Invoice Cloud'
       //    X'00000000999F'
       //    'Y'
       //    'Y'))
       //  JOB(TESTARBAL)
       //  JOBQ(QPGMR)
     D/copy wPrAr
     D/copy sprSql
     DArBal            pr                  Extpgm('TESTARBAL')
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D inMode                        25    const options(*nopass)
     D inInvoice#                    10  0 const options(*nopass)
     D inIncludeBatch                 1    const options(*nopass)
     D inExcludeDeposit...
     D                                1    const options(*nopass)
     DArBal            pi
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D inMode                        25    const options(*nopass)
     D inInvoice#                    10  0 const options(*nopass)
     D inIncludeBatch                 1    const options(*nopass)
     D inExcludeDeposit...
     D                                1    const options(*nopass)

       Dcl-S decRetAmt Packed(15:2);

       decRetAmt =
        ARBalance
        (
        inDt7From:
        inDt7To:
        inCo#:
        inFnd:
        inFyr:
        inEmp#:
        inDiv:
        inMode:
        inInvoice#:
        inIncludeBatch:
        inExcludeDeposit
        );

       Return;
