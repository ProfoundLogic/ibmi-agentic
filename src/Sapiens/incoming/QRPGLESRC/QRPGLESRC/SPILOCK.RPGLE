     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     hNomain
     hOption(*nodebugio:*srcstmt)
      //======================================================
      // Description:
      // This service program is in WTBNDDIR and STBNDDIR.
      // SPRLOCK contains the prototypes needed to implement.
      // This service program has the procedures to:
      //  SetPolicyLock - Writes a record to SXLOCK. LOKEY has
      //   the value to lock the policy or claim.
      //  UpdatePolicyLock - changes the data for the locking
      //   record in case a submitted job submits another job
      //   like Midterm Endorsement Revised Billing.
      //  GetPolicyLockStatus - Checks for a policy lock record
      //   and returns *Off or *On with the job information
      //   in the returning parms.
      //  ClearPolicyLock - Deletes the locking record to
      //   release the lock.
      //======================================================
      // Revisions:
      //
      //
      // Created for WC2187 on 01/23/20 in V20R0
      //======================================================
     fsxlock    uf a e           k disk    rename(pLock:pLock) usropn
     d psds          esds                  extname(szq1p)
      /Copy SprJobInfo
      /Copy WprUsrPrms
      /Free
      //======================================================
       Dcl-Proc ClearPolicyLock Export;
      //======================================================
       Dcl-Pi *N;
        inCo# Packed(3:0);
        inFnd Packed(3:0);
        inEmp Packed(9:0);
       End-Pi;
       Dcl-S strKey Like(LOkey);
       strKey = %Editc(inCo#:'X') + %Editc(inFnd:'X') + %Editc(inEmp:'X');
       If Not(%Open(SXLOCK));
          Open SXLOCK;
       Endif;
       Dou Not(%Found);
          Delete strKey pLock;
       EndDo;
       Close SXLOCK;
       Return;
       End-Proc;
      //======================================================
       Dcl-Proc GetPolicyLockStatus Export;
      //======================================================
       Dcl-Pi *N Ind;
        inCo# Packed(3:0);
        inFnd Packed(3:0);
        inEmp Packed(9:0);
        outJobInfo Char(28);
        outProcess Char(10) Options(*Nopass);
        outUser Char(50) Options(*Nopass);
       End-Pi;
       Dcl-S chrJobN Char(6);
       Dcl-S strKey Like(LOkey);
       strKey = %Editc(inCo#:'X') + %Editc(inFnd:'X') + %Editc(inEmp:'X');
       If Not(%Open(SXLOCK));
          Open SXLOCK;
       Endif;
       Chain(n) strKey pLock;
       Close SXLOCK;
       If Not(%Found);
          Return *Off;
       Endif;
       chrJobN = %EditC(LOjob#:'X');
       If %Parms > 4;
          outProcess = LOproc;
       Endif;
       If %Parms > 5;
          outUser = GetUsrName(LOuser);
       Endif;
       Select;
       When ChkJobActive(LOjob:LOuser:chrJobn) = *On;
          outJobInfo =  chrJobn + '/' + %Trim(LOuser) + '/' +  %Trim(LOjob);
          Return *On;
       When ChkJobJobQ(LOjob:LOuser:chrJobn) = *On;
          outJobInfo =  chrJobn + '/' + %Trim(LOuser) + '/' +  %Trim(LOjob);
          Return *On;
       When ChkJobActive(LOjob:LOcusr:chrJobn) = *On;
          outJobInfo =  chrJobn + '/' + %Trim(LOcusr) + '/' +  %Trim(LOjob);
          Return *On;
       When ChkJobJobQ(LOjob:LOcusr:chrJobn) = *On;
          outJobInfo =  chrJobn + '/' + %Trim(LOcusr) + '/' +  %Trim(LOjob);
          Return *On;
       Other;
          ClearPolicyLock(inCo#:inFnd:inEmp);
          Return *Off;
       EndSl;
       End-Proc;
      //======================================================
       Dcl-Proc SetPolicyLock Export;
      //======================================================
       Dcl-Pi *N;
        inCo# Packed(3:0);
        inFnd Packed(3:0);
        inEmp Packed(9:0);
        process Char(10) Const;
       End-Pi;
       ClearPolicyLock(inCo#:inFnd:inEmp);
       LOproc = process;
       LOkey  = %Editc(inCo#:'X') + %Editc(inFnd:'X') + %Editc(inEmp:'X');
       LOjob# = Q1jobn;
       LOjob  = Q1job;
       LOuser = Q1user;
       LOcusr = Q1cusr;
       LOdate = %Dec(%Char(%Date():*Cymd0):7:0);
       LOtime = %Dec(%Char(%Time():*Hms0):6:0);
       If Not(%Open(SXLOCK));
          Open SXLOCK;
       Endif;
       Write pLock;
       Close SXLOCK;
       Return;
       End-Proc;
      //======================================================
       Dcl-Proc UpdatePolicyLock Export;
      //======================================================
       Dcl-Pi *N;
        inCo# Char(3);
        inFnd Char(3);
        inEmp Char(9);
        inJob# Char(6);
        inJobName Char(10);
       End-Pi;
       Dcl-S strKey Like(LOkey);
       strKey = inCo# + inFnd + inEmp;
       If Not(%Open(SXLOCK));
          Open SXLOCK;
       Endif;
       Chain strKey pLock;
       If %Found;
          LOjob# = %Int(inJob#);
          LOjob  = inJobName;
          LOdate = %Dec(%Char(%Date():*Cymd0):7:0);
          LOtime = %Dec(%Char(%Time():*Hms0):6:0);
          Update pLock;
       Endif;
       Close SXLOCK;
       Return;
       End-Proc;
      /End-Free
