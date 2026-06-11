     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR04302B - CA - Partnership Coverage Exclusion Endorsment
      //===============================================================*
      // WC040302B                                                     *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      // ------------------------------------------------------------- *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
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
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)

      /COPY SCOPYEN01F

      /COPY SCOPYEN01D

     D ix              s              2  0
     D x               s              2  0
     D xxFrst          s              1
     D xxnm            s             40
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     D #1Flag          s              1
     D xxdt            s              7  0

     D  NMDS           ds
     D  FMC401
     D  FMC402
     D  FMC403
     D  FMC404
     D  FMC405
     D  FMC406
     D  FMC407
     D  FMC408
     D  FMC409
     D  FMC410
     D  NM                                 OVERLAY(NMDS)
     D                                     LIKE(FMC401) DIM(10)

     D  TLDS           DS
     D  FMC301
     D  FMC302
     D  FMC303
     D  FMC304
     D  FMC305
     D  FMC306
     D  FMC307
     D  FMC308
     D  FMC309
     D  FMC310
     D  TL                                 OVERLAY(TLDS)
     D                                     LIKE(FMC301) DIM(10)

     D/COPY WWCPOLDAT4

     D qqForm          s             15A   Inz('WC040302B ')

     D addpgm          c                   const('SRXXXXXXXX')
     d qqMod2          s              1
     d qqRRN           s              9  0

     D SR04302B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1

     D SR04302B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq302b                        1

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
       BegSr OthInf;

          ix = *Zeros;
          i = *zeros;
          Clear E04302;
          xxFrst = *Blanks;
          Clear nm;
          Clear tl;

          WriteInCopybook = *Off;

          srDti = dsDate;
          ConvertDate(srDti:*Blanks:'*CYMD':srDto:srErr);

          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) wdell1;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) wdell1;

          Dow Not %EOF(wdell1);

             Chain (ELco#:ELfnd:ELemp#:ELdiv) wmeml;

             If %Found(wmeml) And EMtpob = '2';
                Chain (xxCo#:ELrtcd) wmrsl;

                If %Found(wmrsl) And RSjrst = ENst;
                   Setll (ELco#:ELfnd:ELemp#:ELdiv) wmool;
                   Reade (ELco#:ELfnd:ELemp#:ELdiv) wmool;

                   Dow Not %EOF(wmool);

                      If OOexfl = 'E' And ((OOexdt = 0) or (OOexdt > ELpfdt))
                             and OOstat <> 'I';
                         xxNm = %Trim(OOofsn) + ' ' + %Trim(OOolsn);
                         x = %lookup(xxNm:nm);

                         If x = *Zeros;
                            ix = ix + 1;
                            nm(ix) = %Trim(xxNm);
                            tl(ix) = %Trim(OOtitl);

                            i = i +1;
                            If i = 1;
                               If OOtitl <> *Blanks;
                                  x2Nm1 = %Trim(OOtitl) + ' ' + %Trim(xxNm);
                               Else;
                                  x2Nm1 = %Trim(xxNm);
                               Endif;
                            Endif;
                            If i = 2;
                               If OOtitl <> *Blanks;
                                  x2Nm2 = %Trim(OOtitl) + ' ' + %Trim(xxNm);
                               Else;
                                  x2Nm2 = %Trim(xxNm);
                               Endif;
                            Endif;
                            If i = 3;
                               If OOtitl <> *Blanks;
                                  x2Nm3 = %Trim(OOtitl) + ' ' + %Trim(xxNm);
                               Else;
                                  x2Nm3 = %Trim(xxNm);
                               Endif;
                            Endif;
                            If i = 3;
                               ChkStack('W1054R':#1Flag);

                               If #1Flag = '0';
                                  xxUnid = *Zeros;
                                  xxdt = 9999999;
                                  Setgt (xxCo#:xxFnd:xxEmp#:xxFyr:xxdt) wxpel1;
                                  Readpe (xxCo#:xxFnd:xxEmp#:xxFyr) wxpel1;
                                  If Not %EOF(wxpel1);
                                     xxUnid = PEunid;
                                  Endif;
                                  x1End# = xxForm;
                                  Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxUnid:x1End#)
                                       wxpel;
                                  If %found(wxpel);
                                     PEdata = E04302;
                                     PEuser = q1User;
                                     PEpgm  = q1Pgm;
                                     PEtime = dsTime;
                                     PEdt = %Int(srDto);
                                     If xxFrst = *blank;
                                        Update ppe01;
                                        xxFrst = 'y';
                                     Else;
                                        PEiusr =  q1User;
                                        PEipgm =  q1Pgm;
                                        PEitim =  q1Time;
                                        PEidt = %Int(srDto);
                                        Write ppe01;
                                     EndIf;
                                     i = 0;
                                     Clear E04302;
                                  Endif;
                               Endif;
                            Endif;
                         EndIf;

                         If ix = 10;
                            Exsr WriteWorkFile;
                            ix = 0;
                            Clear nm;
                            Clear tl;
                         Endif;

                      Endif;

                      Reade (ELco#:ELfnd:ELemp#:ELdiv) wmool;
                   EndDo;

                EndIf;

             EndIf;

             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) wdell1;
          Enddo;

          If ix <> *zero;
             Exsr WriteWorkFile;
          EndIf;

          If i <> 0;
             ChkStack('W1054R':#1Flag);

             If #1Flag = '0';
                xxUnid = *Zeros;
                xxdt = 9999999;
                Setgt (xxCo#:xxFnd:xxEmp#:xxFyr:xxdt) wxpel1;
                Readpe (xxCo#:xxFnd:xxEmp#:xxFyr) wxpel1;
                If Not %EOF(wxpel1);
                   xxUnid = PEunid;
                Endif;

                x1End# = xxForm;
                Chain (xxCo#:xxFnd:xxEmp#:xxFyr:xxUnid:x1End#)
                  wxpel;
                If %found(wxpel);
                   PEdata = E04302;
                   PEuser = q1User;
                   PEpgm  = q1Pgm;
                   PEtime = dsTime;
                   PEdt = %Int(srDto);
                   If xxFrst = *blank;
                      Update ppe01;
                      xxFrst = 'y';
                   Else;
                      PEiusr =  q1User;
                      PEipgm =  q1Pgm;
                      PEitim =  q1Time;
                      PEidt = %Int(srDto);
                      Write ppe01;
                   EndIf;
                Endif;

             Endif;

          Endif;

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

          If q1prms >= 7;
             qq302b = *Off;
          Endif;

          Chain (ELco#:ELfnd:ELemp#:ELdiv) wmeml;

          If %Found(wmeml) And EMtpob = '2';

                Setll (ELco#:ELfnd:ELemp#:ELdiv) wmool;
                Reade (ELco#:ELfnd:ELemp#:ELdiv) wmool;

                Dow Not %EOF(wmool);

                   If OOexfl = 'E'
                     And ((OOexdt = 0) or (OOexdt > ELpfdt))
                     And OOstat <> 'I';

                      If q1prms >= 7;
                         qq302b = *On;
                      Endif;

                      crtFrm = *On;
                      Leave;
                   EndIf;

                   Reade (ELco#:ELfnd:ELemp#:ELdiv) wmool;
                Enddo;

          Endif;

       Endsr;
      /End-Free
