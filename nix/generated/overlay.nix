self: super: {
  diagnostica = self.callPackage ./diagnostica {};
  diagnostica-sage = self.callPackage ./diagnostica-sage {};
  sage = self.callPackage ./sage {};
}
