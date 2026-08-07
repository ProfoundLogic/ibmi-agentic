       // ==================================================================
       //  GTIMG -- image capture.
       //
       //  Stores a base64 JPEG from the device camera as a journaled BLOB in
       //  GTIMAGE (the system of record) AND writes the servable file, so the
       //  photo appears the moment it is taken rather than waiting for an
       //  export run.
       //
       //  7-space indent, no conditional directives -- see gtbar_pr.rpgle.
       // ==================================================================

       // Add a captured image. Returns the new image_id, or 0 on failure.
       dcl-pr gtimg_add int(10);
         refType  char(4)       const;
         refKey   char(20)      const;
         b64      varchar(32000) const;
         capEn    varchar(60)   const;
         capFr    varchar(60)   const;
       end-pr;

       // Write an image_id's BLOB out to the static serving path. Called by
       // gtimg_add, and separately usable to re-materialise a lost cache.
       dcl-pr gtimg_publish ind;
         imageId int(10) const;
       end-pr;
