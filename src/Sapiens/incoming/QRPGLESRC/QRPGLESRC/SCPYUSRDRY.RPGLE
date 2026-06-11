     H*****************************************************************
     H***copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H*** StoneRiver is a trademark of StoneRiver, Inc.')
     H***/TITLE SCPYUSRDRY - Copybook for shared def between STUSRDRY and STUSRASGN
     H*****************************************************************
      *
     d conSize         c                   const(20)
      *
     dOpenCursor       pr              n
     d inType                         1    const
      *
     dFetchNextClm     pr             5
     d ptrToDS                         *
      *
     dFetchNextEmp     pr             5
     d ptrToDS                         *
      *
     dCloseCurSor      pr
      *
     dBuildSQLs        pr
     d inStaffCd                      3    const
     d inEmpFld                      10    const
     d inClmFld                      10    const
      *
     dLoadFieldArray   pr
     d inCo#                          3  0 const
     D ptrToEmpFld                     *
     D ptrToEmpDsc                     *
     D ptrToClmFld                     *
     D ptrToClmDsc                     *
      *
     dGetSQLCount      pr             9  0
     d inType                         1    const
      *
     dGetDartCount     pr             9  0
     d inUser                        10    const
     d inFromDate                     7  0 const
     d inOperator                     2    const
     d inToDate                       7  0 const options(*nopass)
     d*
     dGetImageCounts   pr             9  0
     d inUser                        10    const
     d inType                         1    const
     d*
     dFetchNextQte     pr             5
     d ptrToDS                         *
     d*
      *
     dGetVisitCount    pr             9  0
     d inUser                        10    const
     d inFromDate                     7  0 const
     d inOperator                     2    const
     d inToDate                       7  0 const options(*nopass)
