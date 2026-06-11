     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR42313 - TX PEO EXECUTIVE EMPLOYEE INCLUSION ENDORSEMENT
      //===============================================================*
      // WC420313  - Texas Professional Employer Organization (PEO)    *
      //             Executive Employee Inclusion Endorsement          *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 08/05/17                                          *
      // Programmer: C:267                                             *
      // Project   : WC2527                                            *
      //             Copied from SR04303A                              *
      //===============================================================*
      // REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
      //                                                               *
     // 10/06/17 C:136 Project: 150WBL                                *
     //                PEO attachment criteria moved to copy code     *
     //                                                               *
      * 09/18/17 C:267 Project: WC2527                                 *
      *                add check for PEO master and combined policy    *
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*
     Fwmool     if   e           k disk

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D
      /COPY WPRPEO
     D*** Beg Del *** 09/20/24 *****************************************
     D*** /Copy WPREMPLR
     D*** End Del *** 09/20/24 *****************************************

     D ix              s              2  0
     D x               s              2  0
     D xxFrst          s              1
     D xxnm            s             40
     D*xxunid          s                   like(peunid)
     D*x1end#          s                   like(peend#)
     D #1Flag          s              1
     D xxdt            s              7  0

     D                 ds
     D  nm                     1    400
     D                                     dim(10)
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     D  FMC406               201    240
     D  FMC407               241    280
     D  FMC408               281    320
     D  FMC409               321    360
     D  FMC410               361    400
      *
     D                 ds
     D  off                    1    200
     D                                     dim(10)
     D  FMC201                 1     20
     D  FMC202                21     40
     D  FMC203                41     60
     D  FMC204                61     80
     D  FMC205                81    100
     D  FMC206               101    120
     D  FMC207               121    140
     D  FMC208               141    160
     D  FMC209               161    180
     D  FMC210               181    200

     D qqForm          s             15A   Inz('WC420313  ')
     D addpgm          c                   const('SRXXXXXXXX')
     d qqMod2          s              1
     d qqRRN           s              9  0

     D SR42313         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1

     D SR42313         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq313                         1

     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  qqPgmName                    10    const
     d  qqFound                       1

     d ConvertDate     pr                  extPgm('SRCVTDAT')
     d srDti                          8  0
     d srFfmt                         8    const
     d srTfmt                         8    const
     d srDto                         10
     d srErr                          1

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr othinf;

          ix = *Zeros;
          i = *zeros;
          Clear nm;
          Clear off;

          WriteInCopybook = *Off;

          srDti = dsDate;
          ConvertDate(srDti:*Blanks:'*CYMD':srDto:srErr);

          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;

          Dow Not %EOF(wdell1);

             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;

             // select records if type of business is either:
             //   1-Sole proprietorship
             //   2-Partnership
             //   3-Corporation

             If %Found(wmeml) And
                (EMtpob = '1' or EMtpob = '2' or EMtpob = '3');
                Chain (xxCo#:ELrtcd) prs01;

                If %Found(wmrsl) And RSjrst = ENst;

                   //Get Client's information
                   dsCLTinfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                   FM2002 = dsCltInfo.name;
                   FMTOBC = emtpob;

                   //Get owners and officers
                   Setll (ELco#:ELfnd:ELemp#:ELdiv) poo01;
                   Reade (ELco#:ELfnd:ELemp#:ELdiv) poo01;

                   Dow Not %EOF(wmool);

                      If OOexfl = 'I' And ((OOexdt = 0) or (OOexdt > ELpfdt))
                             and OOstat <> 'I';
                         xxNm = %Trim(OOofsn) + ' ' + %Trim(OOolsn);
                         x = %lookup(xxNm:nm);

                         If x = *Zeros;
                            ix = ix + 1;
                            nm(ix) = %Trim(xxNm);
                            Off(ix) = %Trim(OOtitl);
                            i = i +1;
                         EndIf;

                         If ix = 10;
                            Exsr WriteWorkFile;
                            ix = 0;
                            clear nm;
                            clear Off;
                         Endif;

                      Endif;

                      Reade (ELco#:ELfnd:ELemp#:ELdiv) poo01;
                   EndDo;

                EndIf;

             EndIf;

             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
          Enddo;

          If ix <> *zero;
             Exsr WriteWorkFile;
          EndIf;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;

       Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
       If %Found(wmeml) And (EMtpob = '1' or EMtpob = '2' or EMtpob = '3');
          Setll (ELco#:ELfnd:ELemp#:ELdiv) poo01;
          Reade (ELco#:ELfnd:ELemp#:ELdiv) poo01;
          Dow Not %EOF(wmool);
             If OOexfl = 'I' And ((OOexdt = 0) or
             (OOexdt > ELpfdt)) And OOstat <> 'I';
                If q1prms >= 7;
                   qq313  = *On;
                Endif;
                crtFrm = *On;
                Leave;
             EndIf;
          Reade (ELco#:ELfnd:ELemp#:ELdiv) poo01;
          Enddo;
       Endif;

       Endsr;
      /End-Free
