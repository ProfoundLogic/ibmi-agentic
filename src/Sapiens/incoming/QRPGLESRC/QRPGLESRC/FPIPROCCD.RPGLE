     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
    H*********************************************************************
    H* Created: 02/15/16 C: 079 Project: 114MXB                          *
    H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
    H* 02/16/18 C: 079 Project: 114MJB                                   *
    H*                 Add Bill Type H(Hospital) processing              *
     H*                                                                   *
    H*********************************************************************
    F*
    F* file declaration
     F*
     Fststl     if   e           k disk
     Ffmrcl1    if   e           k disk
     Ffmprl     if   e           k disk    usropn
     f*** Beg Add *** 02/16/18 ***************************************
     Ffmsvl1    if   e           k disk    usropn
     Ffacgl1pa  if   e           k disk
     f*** End Add *** 02/16/18 ***************************************
    D*
    D* procedure declaration
     D*
     D RunCommand      pr                  extPgm('QCMDEXC')
     D inCmd                         56    const
     D inCmdLen                      15  5 const

    D* copybook declaration
     D*
     D/copy sprdate
     D/copy sprusrprms
     D/copy fprproccd

    D* array declaration
     D*** Beg Chg *** 02/16/18 ***************************************
     D*** qca             s             56    dim(2) ctdata perrcd(1)
     D qca             s             56    dim(4) ctdata perrcd(1)
     D*** End Chg *** 02/16/18 ***************************************

    D* data structures declaration
     D*
     D psds          esds                  extname(szq1p)

     D                 ds
     D qqcmd                   1     56
     D qqst                   38     39

    D* standalone variables
     D*
     D inCmd           s             56
     D inCmdLen        s             15  5
     D xxkey           s                   like(STkey) inz
     D xxco#           s              3  0 inz
     D*** Beg Add *** 02/16/18 ****************************************
     d inCommonSvc     s             11
     D*** End Add *** 02/16/18 ****************************************

    P*
    P*====================================================================
    P* GetProcCdDes - Get procedure code description
    P*
    P*  GetProcCdDes - returns description for procedure code
    P*   billtype - accepts the type of bill
    P*   proccode - accepts the procedure code to be searched
    P*   adjstate - accepts the adjudication state for the bill
    P*   servdate - accepts the service date for the bill/code
    P*   type     - accepts the service type for the bill/code
    P*   Med#     - accepts the Medicare Number for the bill/code
    P*   desc     - returns the description for the procedure code
    P*====================================================================
    P*
     P GetProcCdDes    b                   export
    P*
     D GetProcCdDes    pi            25
     D inbilltype                     1    const
     D inproccode                    25    const
     D inadjstate                     2    const
     D inservdate                     7S 0 const
     d*** Beg Add *** 02/16/18 ***************************************
     D inType                         1    const
     D inMed#                        10    const
     d*** End Add *** 02/16/18 ***************************************
     D outdesc                       25
    
     C/free
    
        // initialize
        outdesc = *blanks;
        xxco# = GetUserInfo(q1user);

        // get the value of Fee Schedule Type
        xxkey = inadjstate;
        Chain(n) xxkey pst01;

        Select;

        // Physician procedure codes

         When inbilltype = 'N';

            If STfsty = 'U' or STfsty = *blanks;
               Chain(n) (xxco#:inproccode) prc01;
               If %found(fmrcl1);
                  outdesc = RCdsc1;
               Endif;
            Endif;

            If STfsty = 'S';

               // override to FMPRLxx
               qqcmd = qca(1);
               qqst = inadjstate;
               inCmd = qqcmd;
               inCmdLen = 56;

               callp RunCommand(inCmd:inCmdLen);

               // open FMPRLxx
               open fmprl;

               // get description
               Setgt (xxco#:inproccode:inservdate) ppr01;
               Readpe (xxco#:inproccode) ppr01;
               If not %eof(fmprl);
                  outdesc = PRdsc1;
               Endif;

               // delete override FMPRLxx
               qqcmd = qca(2);
               inCmd = qqcmd;
               inCmdLen = 56;

               callp RunCommand(inCmd:inCmdLen);

               close fmprl;

            Endif;
        //*** Beg Add *** 02/16/18 ***************************************

        // Hospital HCPCS codes

         When inbilltype = 'H' and inType = 'H';

            If STfsty = 'U' or STfsty = *blanks;
               Chain(n) (xxco#:inproccode) prc01;
               If %found(fmrcl1);
                  outdesc = RCdsc1;
               Endif;
            Endif;

            If STfsty = 'S';

               // override to FMPRLxx
               qqcmd = qca(1);
               qqst = inadjstate;
               inCmd = qqcmd;
               inCmdLen = 56;

               callp RunCommand(inCmd:inCmdLen);

               // open FMPRLxx
               open fmprl;

               // get description
               Setgt (xxco#:inproccode:inservdate) ppr01;
               Readpe (xxco#:inproccode) ppr01;
               If not %eof(fmprl);
                  outdesc = PRdsc1;
               Endif;

               // delete override FMPRLxx
               qqcmd = qca(2);
               inCmd = qqcmd;
               inCmdLen = 56;

               callp RunCommand(inCmd:inCmdLen);

               close fmprl;

            Endif;

        // Hospital Revenue codes

         When inbilltype = 'H' and inType = 'R';

               // override to FMSVL1xx
               qqcmd = qca(1);
               qqst = inadjstate;
               inCmd = qqcmd;
               inCmdLen = 56;

               callp RunCommand(inCmd:inCmdLen);

               // open FMSVL1xx
               open fmsvl1;

               // get description
               inCommonSvc = inProcCode;
               Setgt (xxco#:inCommonSvc:inservdate) psv01;
               Readpe (xxco#:inCommonSvc) psv01;
               If not %eof(fmsvl1) and %Subst(SVcmsv:1:3) = inCommonSvc;
                  outdesc = SVdesc;
               Endif;

               // delete override FMSVL1xx
               qqcmd = qca(2);
               inCmd = qqcmd;
               inCmdLen = 56;

               callp RunCommand(inCmd:inCmdLen);

               close fmsvl1;

        // Hospital ChargeMaster Service Codes

         When inbilltype = 'H' and inType = 'C';

            Setgt (inMed#:inProcCode:inservdate) pcg01;
            Readpe (inMed#:inProcCode) pcg01;
            If Not %eof(facgl1pa);
               outdesc = CGdesc;
            Endif;
        //*** End Add *** 02/16/18 ***************************************

         Other;

        Endsl;
    
        // return the description
        return outdesc;
    
      /end-free
     C*
     P GetProcCdDes    e

**  TABLE QCA (QCMDEXC)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  ) OVRSCOPE(*JOB)
DLTOVR FILE(FMPRL) LVL(*JOB)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  ) OVRSCOPE(*JOB)
DLTOVR FILE(FMSVL1) LVL(*JOB)
