     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve file member list
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STFILDOWN
     h*  Calls to:
     h*
     h*  Created on 04/03/2013 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
      *
      * 12/16/22 C:300 Support: 105570 (UBIC)
      *                Return full list of members.
      *
      * 01/27/17 C:062 Support: 44930(099)
      *                Expand the number of members.
      *
      * 05/27/15 C:062 Support: 34996(129)
      *                Change sort order and hanble cases where the number
      *                of members exceeds the size of the array.
      *
      * 08/13/14 C:180 Support: WA129 86371
      *                Allow more members to be displayed.
      *
      **********************************************************************
      /copy sPrAtrium
      /copy sPrString
      * *ENTRY
     DMemberList       pr                  ExtPgm('STFILDOW01')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DMemberList       pi
     D inRrn                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1
     D*
     D* Create User Space API
     D*
     D CrtUsrSpc       PR                  ExtPgm('QUSCRTUS')
     D   UsrSpc                      20A   CONST
     D   ExtAttr                     10A   CONST
     D   InitSiz                     10I 0 CONST
     D   InitVal                      1A   CONST
     D   PubAuth                     10A   CONST
     D   Text                        50A   CONST
     D   Replace                     10A   CONST
     D   ErrorCode                  256A
     D*
     D* Retrieve Pointer to User Space API
     D*
     D RtvPtrUS        PR                  ExtPgm('QUSPTRUS')
     D   UsrSpc                      20A   CONST
     D   PtrUsrSpc                     *

     D* Sub-Procedure to Offset a Pointer (LOCAL)
     D*
     D OffsetPtr       PR              *
     D   pePointer                     *
     D   peOffset                    10I 0 Value

     D* API Error Code Structure
     D*
     D dsEC            DS
     D*                                    Bytes Provided (size of struct)
     D  dsECBytesP             1      4B 0
     D*                                    Bytes Available (returned by API
     D  dsECBytesA             5      8B 0
     D*                                    Msg ID of Error Msg Returned
     D  dsECMsgID              9     15
     D*                                    Reserved
     D  dsECReserv            16     16
     D*                                    Msg Data of Error Msg Returned
     D  dsECMsgDta            17    256

     D* List Header Data Structure
     D*
     D p_LH            S               *
     D dsLH            DS                   BASED(p_LH)
     D*                                     Filler
     D   dsLHFill1                  103A
     D*                                     Status (I=Incomplete,C=Complete
     D*                                             F=Partially Complete)
     D   dsLHStatus                   1A
     D*                                     Filler
     D   dsLHFill2                   12A
     D*                                     Header Offset
     D   dsLHHdrOff                  10I 0
     D*                                     Header Size
     D   dsLHHdrSiz                  10I 0
     D*                                     List Offset
     D   dsLHLstOff                  10I 0
     D*                                     List Size
     D   dsLHLstSiz                  10I 0
     D*                                     Count of Entries in List
     D   dsLHEntCnt                  10I 0
     D*                                     Size of a single entry
     D   dsLHEntSiz                  10I 0

     D* List Members (QUSLMBR) API
     D ListMbrs        PR                  ExtPgm('QUSLMBR')
     D   UsrSpc                      20A   CONST
     D   Format                       8A   CONST
     D   DataBase                    20A   CONST
     D   Member                      10A   CONST
     D   Override                     1A   CONST
     D   ErrorCode                  256A

     D* Data Section of List for QUSLMBR API
     D*
     D p_MbrName       S               *
     D MbrName         S             10A   BASED(p_MbrName)
     ** Begin add *** 08/13/14 ********************************************
     D p_MbrDate       S               *
     D MbrDate         S             13A   BASED(p_MbrDate)
     D IX              S              5  0 inz
     D                 DS
     D*** Begin Chg *** 05/27/15 ******************************************
     D*@MBR*****                     23    DIM(300) DESCEND
     D*** Begin Chg *** 1/27/17 *******************************************
     D*@MBR*****                     23    DIM(2000) DESCEND
     D @MBR                          23    DIM(10000) DESCEND
     D*** End   Chg *** 1/27/17 *******************************************
     D*** End   Chg *** 05/27/15 ******************************************
     D  @MbrDate                     13    Overlay(@MBR)
     D  @MbrName                     10    Overlay(@MBR:14)
     ** End   add *** 08/13/14 ********************************************

     D Offset          S             10I 0
     D*
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz
     D Entry           s              4  0 inz
     D FileLib         s             20    inz
     D library         s             10    inz
     D filename        s             10    inz
     D*** begin Chg *** 12/16/22 ******************************************
     D*** choices         s           1000    inz
     D choices         s          32000    inz
     D json            s               a   len(8500000) varying
     D*** End   Chg *** 12/16/22 ******************************************
      /Free

       //                          1   5  8  9
       // input data is in format: ,library,file,

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;
       FileLib = filename + library;
       FileLib = upper(FileLib);
       choices = *blanks;

       Monitor;
       // set length of Error Code Structure
       dsECBytesP = 256;
       // create a user space
       CrtUsrSpc('MBRDATA   QTEMP':'USRSPC':1:x'00':'*ALL':
        'List of Members in File':'*YES': dsEC);
       // list members into user space
       //*** Begin Change 08/13/14 **************************************
       //***ListMbrs('MBRDATA   QTEMP':'MBRL0100':FileLib:'*ALL':'0':dsEC);
       ListMbrs('MBRDATA   QTEMP':'MBRL0200':FileLib:'*ALL':'0':dsEC);
       //*** End   Change 08/13/14 **************************************
       //get a pointer to the start
       //    of the user space...
       RtvPtrUS('MBRDATA   QTEMP':p_LH);

       // Loop through all the
       //  list entries returned.
       //*** Begin add 08/13/14 **************************************
       clear @MBR;
       //*** End   add 08/13/14 **************************************
       For Entry = 1 to dsLHEntCnt;
          //calculate the number of bytes from the start
          //of the user space, and put our pointer there...
           Offset = ((Entry-1) * dsLHEntSiz)
                        + dsLHLstOff;
           p_MbrName = OffsetPtr(p_LH: Offset);
        //*** Begin delete 08/13/14 **********************************
        //   If choices = *blanks;
        //     choices = %Trim(MbrName);
        //   Else;
        //     choices = %trim(choices) + ',' + %Trim(MbrName);
        //   Endif;
        //*** End   delete 08/13/14 **********************************

       //*** Begin add 08/13/14 **************************************
           p_MbrDate = P_LH + Offset + 20;
           @MBR(Entry) = MbrDate + %trim(MbrName);
       //*** End   add 08/13/14 **************************************
       EndFor;
       //*** Begin add 08/13/14 **************************************
       sorta @MbrDate;
       For IX = 1 to dsLHEntCnt;
           If choices = *blanks;
             choices = %trim(@MbrName(IX));
           Else;
             choices = %trim(choices) + ',' + %Trim(@MbrName(IX));
           Endif;
       Endfor;

       //*** End   add 08/13/14 **************************************

       //*** Begin chg 12/16/22 **************************************
       //***** BuildJSON(outJSON:'choices':%trim(choices));
       BuildGiantJSON(json:'choices':%trim(choices));
       outJSON = %trim(json);
       //*** End   chg 12/16/22 **************************************
       RtnCode = ' ';
       On-Error;
         BuildJSON(outJSON :'RtnCode' :'Member list not retrieved');
       EndMon;

       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Monitor;
       // extract library
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         library = %trim(value);
       // extract filename
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         filename = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
       //*********************************************************************
       // extract values from genparm
       //*********************************************************************
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         If endPos > 0;
           len = endPos-startPos;
           If len > 0;
             value = %Subst(inGenParm:startPos:len);
           Endif;
         Endif;
       Endif;
       EndSr;
      /End-Free
     P*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P* Return a pointer at a specified offset value from another ptrr ptr
     P*
     P*  (Since IBM hates me and won't let me do pointer arithmeticmetic
     P*   without spending a fortune on a RISC box) box)
     P*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P OffsetPtr       B
     D OffsetPtr       PI              *
     D   pePointer                     *
     D   peOffset                    10I 0 Value

     D p_NewPtr        S               *
     D wkMove          S              1A   DIM(4097) BASED(p_NewPtr)

     c                   eval      p_NewPtr = pePointer
     c                   if        peOffset > 0

     C                   dow       peOffset > 4096
     C                   eval      p_NewPtr = %addr(wkMove(4097))
     c                   eval      peOffset = peOffset - 4096
     c                   enddo

     C                   eval      p_NewPtr = %addr(wkMove(peOffset+1))

     c                   endif

     c                   return    p_NewPtr
     P                 E
