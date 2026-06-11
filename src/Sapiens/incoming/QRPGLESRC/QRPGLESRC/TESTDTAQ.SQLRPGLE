     D SendData        PR                  ExtPgm('QSNDDTAQ')
     D   Dtaqnam                     10a   const
     D   Dtaqlib                     10a   const
     D   Dtaqlen                      5p 0 const
     D   Data                              const like(data)

     D ReceiveData     PR                  ExtPgm('QRCVDTAQ')
     D   Dtaqnam                     10a   const
     D   Dtaqlib                     10a   const
     D   Dtaqlen                      5p 0 const
     D   Data                              like(data)
     D   WaitTime                     5p 0 const

     D dataQueue       s             10a   Inz('SIWEBSVC')
     D dataLib         s             10a   Inz('PDSOBJ099')
     D dataLen         s              5p 0 Inz(64000)
     D data            s          64000a   Inz
     d waitTime        s              5p 0 Inz(1)

     d noData          s               n   Inz
     d json            s                   SqlType(clob:64000)

       DoU data = ' ';
          // Get data queue entry
          Monitor;
             data = ' ';
             ReceiveData(dataQueue:dataLib:64000:data:waitTime);
          On-Error;
             noData = *On;
          EndMon;

          // Write entry to file
          If data <> ' ';
             json_data = data;
             json_len  = %Len(%Trim(data));
             Exec Sql Insert into SIWEBSVC Values(:json);
          EndIf;
       EndDo;

       *inLR =*On;
