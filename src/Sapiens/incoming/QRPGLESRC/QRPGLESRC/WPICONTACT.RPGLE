     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
     H**************************************************************************************
     H* Created                                                                            *
     H*                                                                                    *
     H* 03/06/12  C:189 Project: WC2115                                                    *
     H*                 Added a procedure to fetch the Email address                      *
     H*                                                                                    *
     H**************************************************************************************
    F*
     Fwmecl1    if   e           k disk    rename(pec01:pec00)

     D/copy wprcontact

     D psds          esds                  extname(szq1p)


     PGetEmpEmail      b                   export

     DGetEmpEmail      PI            60
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D pcnt                           1                     const
      *
      *
     C     keyWMECL1     Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    emp#
     C                   Kfld                    div
     C                   Kfld                    Pcnt
      /Free
       Chain keyWMECL1 pec00;
       If %Found(wmecl1);
          If ecfsn<>*Blanks;
             return ECeMal;
          EndIf;
       EndIf;
       return *blanks;

      /End-Free

     PGetEmpEmail      e
