      /TITLE ENF001 - /COPY 'F' Specs for Endorsement programs
       //===============================================================*
       //  Revisions:                                                   *
       //                                                               *
       // 01/31/20 C:136 Support: 77734                                 *
       //                Add WXFMPMULTI for multiple entries on         *
       //                schedule.                                      *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2 changes                      *
       //                Clean up program.                              *
       //                                                               *
       //===============================================================*
     fwdell     if   e           k disk    Infds(elfds)
     fwdell1    if   e           k disk    Rename(pel01:pel02)
     fwdf2l     if   e           k disk    Usropn
     fwmaal1    if   e           k disk    Usropn
     fwmagl     if   e           k disk    Usropn
     fwmahl     if   e           k disk    Usropn
     fwmeml     if   e           k disk
     fwmfnl     if   e           k disk    Usropn
     fwmmul4    if a e           k disk    Usropn infds(mufds4)
     fwmmul     if   e           k disk    Rename(pmu01:pmu02)
     fwmrpl     if   e           k disk    Usropn
     fwmrsl     if   e           k disk
     fwtenl     if   e           k disk
     fwxfmp     o    e             disk    Usropn
     fwxicp     o    e             disk    Usropn
      * Endorsement States - Approved
     fwtesl     if   e           k disk
      * Scriptura output file
     fxmlprel1  uf   e           k disk
     fxmlteml1  uf   e           k disk
     fwxwxfmp   o    e             disk    Usropn
     fwxfmpmultio    e             disk    Usropn Rename(fmrecord:fmmulti)
