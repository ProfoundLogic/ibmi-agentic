     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA028R - API to Access WTENP / Return PROC PGMS & FOLDER
      *===============================================================*
      *  DESCRIPTION:                                                 *
      *    This program will access the Endorsement Forms Table file  *
      *    for a givven key, and return processing pgms 1/2/3 & folder*
      *                                                               *
      *  CREATED: 11/26/97  C:57  Project:  CM027                     *
      *===============================================================*
      *  REVISIONS:                                                   *
      *                                                               *
      * 05/08/18 C:65  Support: PHW#70797 (SCS)                       *
      *                Trim suffix value before appending folder.     *
      *                                                               *
      * 12/05/17 C:65  Support: 150WCD (Rewrite)                      *
      *                Update form # to be 15A in size.               *
      *                                                               *
      * 07/20/17 C:136 Support: 48310 (Rewrite)                       *
      *                Prevent midterm endorsements from attaching    *
      *                expired endorsements                           *
      *                                                               *
      *===============================================================*
     fwtenl     if   e           k disk
     fwdell     if   e           k disk    UsrOpn

      *Entry
     d wa028r          pr
     d                                3
     d                                3
     d                               15
     d                               10
     d                               10
     d                               10
     d                               32
     d                                1
     d                                9
     d                                5
     d                                3
     d wa028r          pi
     d  @@co#                         3
     d  @@fnd                         3
     d  @@form                       15
     d  @@pgm1                       10
     d  @@pgm2                       10
     d  @@pgm3                       10
     d  @@fldr                       32
     d  @@rtcd                        1
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3

     d GetSuffix       pr                  extPgm('SA030R')
     d  suffix                        3
     d  prefix                       80

     d hasPgmFldr      s               n
     d strSuffix       s              3
     d strPrefix       s             80

      /Free
       Chain (%Int(@@co#):%Int(@@fnd):@@form) pen01;
       Select;
       When Not(%Found);
          hasPgmFldr = *Off;
       When %Parms > 8;
          Open wdell;
          Chain (%Int(@@co#):%Int(@@fnd):%Int(@@Emp#):
           %Int(@@Div):%Int(@@Fyr)) pel01;
          Select;
          When ELPfDt >= ENEdDt and ENRpDt = 0 or
           ELPfDt >= ENEdDt and ELPfDt < ENRpDt;
             hasPgmFldr = *On;
          Other;
             hasPgmFldr = *Off;
          Endsl;
          Close wdell;
       Other;
          hasPgmFldr = *On;
       Endsl;

       If hasPgmFldr = *On;
          @@pgm1 = ENpgm1;
          @@pgm2 = ENpgm2;
          @@pgm3 = ENpgm3;
          @@rtcd = *blank;
          GetSuffix(strSuffix:strPrefix);
          If strPrefix <> *blank;
          //*** begin chg ***  05/08/18  *************************************
          //*****   @@fldr = strPrefix + '/' + ENfldr;
             @@fldr = %trim(strPrefix) + '/' + %trim(ENfldr);
          //*** begin chg ***  05/08/18  *************************************
          Else;
             @@fldr = ENfldr;
          Endif;
       Else;
          @@pgm1 = *blank;
          @@pgm2 = *blank;
          @@pgm3 = *blank;
          @@fldr = *blank;
          @@rtcd = *blank;
       Endif;

       Return;

      /End-Free
