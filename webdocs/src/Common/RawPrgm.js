// Readers of the raw program the compiler sends: a page's statements, and the metas on its
// expressions, where each value the compiler computed for the page rides (as `getMetaDat[2]`).
//
// In a module of their own, importing nothing, so that whatever reads a page reads it the same way:
// the webdocs, and the integration suite that checks the server against them. The suite runs outside
// the bundler, where a module reaching the editor through the displays cannot load, and a copy of
// these kept there is how it came to read a shape the compiler had stopped sending.

// The meta of a raw expression, whichever kind it is.
//
// The three that wrap another expression without carrying a meta of their own hand back the meta of
// what they wrap, which is the expression a value shown here would belong to.
function rawExprMeta(expr) {
  switch(expr.tag) {
  case "RawCExpr":        return expr.rceMeta;
  case "RawValue":        return expr.rvMeta;
  case "RawHoleExpr":     return expr.rheMeta;
  case "RawMacroValue":   return expr.rmvMeta;
  case "RawApplyExpr":    return expr.raeMeta;
  case "RawFmtStrExpr":   return expr.rfsMeta;
  case "RawTupleApply":   return expr.rtaMeta;
  case "RawVarsApply":    return expr.rvaMeta;
  case "RawContextApply": return expr.rcaMeta;
  case "RawWhere":        return expr.rwMeta;
  case "RawMethod":       return expr.rmMeta;
  case "RawList":         return expr.rlMeta;
  case "RawTypeProp":     return expr.rtpMeta;
  case "RawTheExpr":      return rawExprMeta(expr.rteExpr);
  case "RawAliasExpr":    return rawExprMeta(expr.raliasBase);
  case "RawParen":        return rawExprMeta(expr.rpnExpr);
  default:
    console.error("Unknown rawExprMeta", expr);
    return null;
  }
}

// What a statement holds: an ObjArr for a declaration or a binding, an expression for an annotation.
//
// Read by field, as everything raw is -- the compiler names the fields of every raw constructor, so
// a positional `contents` is not what arrives. See `rawExprMeta` above.
function statementBody(statement) {
  switch (statement.tag) {
  case "RawDeclStatement": return statement.rdsObjArr;
  case "RawBindStatement": return statement.rbsObjArr;
  case "RawAnnot":         return statement.ranAnnot;
  default:
    console.error("Unknown statement", statement);
    return undefined;
  }
}

export {rawExprMeta, statementBody};
