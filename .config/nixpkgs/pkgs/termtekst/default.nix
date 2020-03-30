# WIP: does not yet work.
{ fetchFromGitHub, pythonPackages }:

pythonPackages.buildPythonApplication rec {
  pname = "termtekst";
  version = "1.0";
  src = fetchFromGitHub {
    owner = "zevv";
    repo = "termtekst";
    rev = "v${version}";
    sha256 = "1gm7j5d49a60wm7px82b76f610i8pl8ccz4r6qsz90z4mp3lyw9b";
  };

  preConfigure = ''
    sed -i 's/assert os.geteuid() == 0, "Please run with root privileges."/pass/g' "setup.py"
  '';

  doCheck = false;

  propagatedBuildInputs = [
    pythonPackages.requests
  ];
}
