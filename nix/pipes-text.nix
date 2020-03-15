{ mkDerivation, base, bytestring, fetchgit, pipes, pipes-bytestring
, pipes-group, pipes-parse, pipes-safe, stdenv, streaming-commons
, text, transformers
}:
mkDerivation {
  pname = "pipes-text";
  version = "0.0.2.5";
  src = fetchgit {
    url = "https://github.com/coot/text-pipes";
    sha256 = "1k72n21sxnvdsmpjdfd5qani5a0vsjx9yiwj0alx6q4pkly44xx8";
    rev = "1e8bd919a8b5b3ec20351340f2495dd65047839c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring pipes pipes-bytestring pipes-group pipes-parse
    pipes-safe streaming-commons text transformers
  ];
  homepage = "https://github.com/michaelt/text-pipes";
  description = "properly streaming text";
  license = stdenv.lib.licenses.bsd3;
}
