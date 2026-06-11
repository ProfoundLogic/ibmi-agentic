      /Free
       Dcl-Pr ClearPolicyLock;
        co# Packed(3:0);
        fnd Packed(3:0);
        emp Packed(9:0);
       End-Pr;
       Dcl-Pr GetPolicyLockStatus Ind;
        co# Packed(3:0);
        fnd Packed(3:0);
        emp Packed(9:0);
        outMsg Char(28);
        outProcess Char(10) Options(*Nopass);
        outUser Char(50) Options(*Nopass);
       End-Pr;
       Dcl-Pr SetPolicyLock;
        co# Packed(3:0);
        fnd Packed(3:0);
        emp Packed(9:0);
        process Char(10) Const;
       End-Pr;
       Dcl-Pr UpdatePolicyLock;
        co# Char(3);
        fnd Char(3);
        emp Char(9);
        job# Char(10);
        jobName Char(10);
       End-Pr;
      /End-Free
