// PSharedFile is a Perspectives property Range type.
// It is a serialised JSON structure:

import { PSharedFile, RoleInstanceT } from "perspectives-proxy";

// export const PSharedFile =
//   { // The file name as it was uploaded.
//     name: string.isRequired
//   , size: number.isRequired
//   // The mime type
//   , type: string
//   // Identification of the Storage service. Will be a role identifier.
//   , sharedStorageId: string.isRequired
//   // Type of storage, e.g. "mega"
//   , storageType: string.isRequired
//   // Whatever is needed to retrieve the file from the Storage identified by sharedStorageId, e.g. a Mega url.
//   , url: string.isRequired
//   }

  interface IFile {
    name: string; // is in File, too.
    size: number; // is in File, too, inherited from Blob.
    type: string; // is in File, too, inherited from Blob.
  }
  
  export function file2PsharedFile(
    theFile: IFile,
    sharedStorageId: RoleInstanceT,
    storageType: string,
    url: string
  ): PSharedFile {
    return {
      name: theFile.name,
      size: theFile.size,
      type: theFile.type,
      sharedStorageId,
      storageType,
      url,
    };
  }