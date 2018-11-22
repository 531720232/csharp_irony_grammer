using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;
using Irony.Parsing;
using System.Globalization;

namespace CVM
{



  [Language("c#", "6", "full c# grammar")]

  public class CSharpGrammar : Grammar
  {
    #region Lexical structure
    StringLiteral StringLiteral = TerminalFactory.CreateCSharpString("StringLiteral");
    StringLiteral CharLiteral = TerminalFactory.CreateCSharpChar("CharLiteral");
    NumberLiteral Number = TerminalFactory.CreateCSharpNumber("Number");
    IdentifierTerminal identifier = TerminalFactory.CreateCSharpIdentifier("Identifier");
    CommentTerminal SingleLineComment = new CommentTerminal("SingleLineComment", "//", "\r", "\n", "\u2085", "\u2028", "\u2029");
    CommentTerminal DelimitedComment = new CommentTerminal("DelimitedComment", "/*", "*/");
    CommentTerminal ppInstruction = new CommentTerminal("ppInstruction", "#", "\n");


    KeyTerm colon;
    KeyTerm semi;
    NonTerminal semi_opt = new NonTerminal("semi?");
    KeyTerm dot;
    KeyTerm comma;
    NonTerminal comma_opt = new NonTerminal("comma_opt");
    NonTerminal commas_opt = new NonTerminal("commas_opt");
    KeyTerm qmark;
    NonTerminal qmark_opt = new NonTerminal("qmark_opt");
    KeyTerm Lbr;
    KeyTerm Rbr;
    KeyTerm Lpar;
    KeyTerm Rpar;
    KeyTerm tgoto;
    KeyTerm yld;

    KeyTerm Lparx;

    #endregion



    #region lex2

    KeyTerm ABSTRACT;
    KeyTerm ADD;
    KeyTerm ALIAS;
    KeyTerm ARGLIST;
    KeyTerm AS;
    KeyTerm ASCENDING;
    KeyTerm ASYNC;
    KeyTerm AWAIT;
    KeyTerm BASE;
    KeyTerm BOOL;
    KeyTerm BREAK;
    KeyTerm BY;
    KeyTerm BYTE;
    KeyTerm CASE;
    KeyTerm CATCH;
    KeyTerm CHAR;
    KeyTerm CHECKED;
    KeyTerm CLASS;
    KeyTerm CONST;
    KeyTerm CONTINUE;
    KeyTerm DECIMAL;
    KeyTerm DEFAULT;
    KeyTerm DELEGATE;
    KeyTerm DESCENDING;
    KeyTerm DO;
    KeyTerm DOUBLE;
    KeyTerm DYNAMIC;
    KeyTerm ELSE;
    KeyTerm ENUM;
    KeyTerm EQUALS;
    KeyTerm EVENT;
    KeyTerm EXPLICIT;
    KeyTerm EXTERN;
    KeyTerm FALSE;
    KeyTerm FINALLY;
    KeyTerm FIXED;
    KeyTerm FLOAT;
    KeyTerm FOR;
    KeyTerm FOREACH;
    KeyTerm FROM;
    KeyTerm GET;
    KeyTerm GOTO;
    KeyTerm GROUP;
    KeyTerm IF;
    KeyTerm IMPLICIT;
    KeyTerm IN;
    KeyTerm INT;
    KeyTerm INTERFACE;
    KeyTerm INTERNAL;
    KeyTerm INTO;
    KeyTerm IS;
    KeyTerm JOIN;
    KeyTerm LET;
    KeyTerm LOCK;
    KeyTerm LONG;
    KeyTerm NAMEOF;
    KeyTerm NAMESPACE;
    KeyTerm NEW;
    KeyTerm NULL;
    KeyTerm OBJECT;
    KeyTerm ON;
    KeyTerm OPERATOR;
    KeyTerm ORDERBY;
    KeyTerm OUT;
    KeyTerm OVERRIDE;
    KeyTerm PARAMS;
    KeyTerm PARTIAL;
    KeyTerm PRIVATE;
    KeyTerm PROTECTED;
    KeyTerm PUBLIC;
    KeyTerm READONLY;
    KeyTerm REF;
    KeyTerm REMOVE;
    KeyTerm RETURN;
    KeyTerm SBYTE;
    KeyTerm SEALED;
    KeyTerm SELECT;
    KeyTerm SET;
    KeyTerm SHORT;
    KeyTerm SIZEOF;
    KeyTerm STACKALLOC;
    KeyTerm STATIC;
    KeyTerm STRING;
    KeyTerm STRUCT;
    KeyTerm SWITCH;
    KeyTerm THIS;
    KeyTerm THROW;
    KeyTerm TRUE;
    KeyTerm TRY;
    KeyTerm TYPEOF;
    KeyTerm UINT;
    KeyTerm ULONG;
    KeyTerm UNCHECKED;
    KeyTerm UNSAFE;
    KeyTerm USHORT;
    KeyTerm USING;
    KeyTerm VAR;
    KeyTerm VIRTUAL;
    KeyTerm VOID;
    KeyTerm VOLATILE;
    KeyTerm WHEN;
    KeyTerm WHERE;
    KeyTerm WHILE;
    KeyTerm YIELD;


    #endregion


    #region NonTerminals

    NonTerminal compilation_unit = new NonTerminal("compilation_unit");
    NonTerminal extern_alias_directive = new NonTerminal("extern_alias_directive");
    NonTerminal extern_alias_directives_opt = new NonTerminal("extern_alias_directives_opt");
    NonTerminal using_directive = new NonTerminal("using_directive");
    NonTerminal using_directives = new NonTerminal("using_directives");
    NonTerminal using_directives_opt = new NonTerminal("using_directives_opt");
    NonTerminal namespace_declaration = new NonTerminal("namespace_declaration");
    NonTerminal namespace_declarations_opt = new NonTerminal("namespace_declarations_opt");
    NonTerminal type_declaration = new NonTerminal("type_declaration");

    NonTerminal qualified_identifier = new NonTerminal("qualified_identifier");
    NonTerminal namespace_body = new NonTerminal("namespace_body");
    NonTerminal namespace_member_declaration = new NonTerminal("namespace_member_declaration");
    NonTerminal namespace_member_declarations = new NonTerminal("namespace_member_declarations");
    NonTerminal namespace_member_declarations_opt = new NonTerminal("namespace_member_declarations_opt");
    NonTerminal using_alias_directive = new NonTerminal("using_alias_directive");
    NonTerminal using_ns_directive = new NonTerminal("using_ns_directive");
    NonTerminal using_st_directive = new NonTerminal("using_static_directive");
    NonTerminal global_attribute_section_opt = new NonTerminal("global_attribute_section_opt");
    NonTerminal global_attribute_section = new NonTerminal("global_attribute_section");
    NonTerminal global_attribute_target = new NonTerminal("global_attribute_target");
    NonTerminal attribute_list = new NonTerminal("attribute_list");
    NonTerminal attribute = new NonTerminal("attribute");
    NonTerminal attributes_opt = new NonTerminal("attributes_opt");



    NonTerminal namespace_or_type_name = new NonTerminal("namespace_or_type_name");
    NonTerminal identifier_or_builtin = new NonTerminal("identifier_or_builtin");
    NonTerminal builtin_type = new NonTerminal("builtin_type");
    NonTerminal integral_type = new NonTerminal("integral_type");
    NonTerminal qual_name_segments_opt = new NonTerminal("qual_name_segments_opt");
    NonTerminal qual_name_segment = new NonTerminal("qual_name_segment");
    NonTerminal type_argument_list = new NonTerminal("type_argument_list");
    NonTerminal gen_lt = new NonTerminal("gen_lt");


    NonTerminal comma_list_opt = new NonTerminal("comma_list_opt");
    NonTerminal rank_specifier = new NonTerminal("rank_specifier");
    NonTerminal rank_specifiers = new NonTerminal("rank_specifiers");
    NonTerminal rank_specifiers_opt = new NonTerminal("rank_specifiers_opt");
    NonTerminal type = new NonTerminal("type");
    NonTerminal type_list = new NonTerminal("type_list");
    NonTerminal type_or_void = new NonTerminal("type_or_void");
    NonTerminal typearg_or_gendimspec_list = new NonTerminal("typearg_or_gendimspec_list");
    NonTerminal generic_dimension_specifier_opt = new NonTerminal("generic_dimension_specifier_opt");

    //修饰符

    NonTerminal all_member_modifiers = new NonTerminal("all_member_modifiers");
    NonTerminal all_member_modifier = new NonTerminal("all_member_modifier");


    //类
    NonTerminal class_definition = new NonTerminal("class_definition");
    NonTerminal struct_definition = new NonTerminal("struct_definition");
    NonTerminal interface_definition = new NonTerminal("interface_definition");
    NonTerminal enum_definition = new NonTerminal("enum_definition");
    NonTerminal delegate_definition = new NonTerminal("delegate_definition");

    NonTerminal type_parameter_list_opt = new NonTerminal("type_parameter_list_opt");
    NonTerminal type_parameter = new NonTerminal("type_parameter");
    NonTerminal type_parameters = new NonTerminal("type_parameters");

    NonTerminal type_parameter_constraints_clause = new NonTerminal("type_parameter_constraints_clause");
    NonTerminal type_parameter_constraints_clauses_opt = new NonTerminal("type_parameter_constraints_clauses");
    NonTerminal type_parameter_constraint = new NonTerminal("type_parameter_constraint");
    NonTerminal type_parameter_constraints = new NonTerminal("type_parameter_constraints");

    NonTerminal class_base = new NonTerminal("class_base");
    NonTerminal class_body = new NonTerminal("class_body");
    NonTerminal class_base_opt = new NonTerminal("class_base_opt");

    NonTerminal class_member_declarations_opt = new NonTerminal("class_member_declarations_opt");
    NonTerminal class_member_declaration = new NonTerminal("class_member_declarations_opt");

    NonTerminal common_member_declaration = new NonTerminal("common_member_declaration");
    NonTerminal destructor_definition = new NonTerminal("destructor_definition");

    NonTerminal constant_declaration = new NonTerminal("constant_declaration");
    NonTerminal typed_member_declaration = new NonTerminal("typed_member_declaration");
    NonTerminal event_declaration = new NonTerminal("event_declaration");
    NonTerminal conversion_operator_declarator = new NonTerminal("conversion_operator_declarator");
    NonTerminal constructor_declaration = new NonTerminal("constructor_declaration");

    NonTerminal conversion_operator_declarator_opt = new NonTerminal("conversion_operator_declarator_opt");

    NonTerminal method_declaration = new NonTerminal("method_declaration");
    NonTerminal method_declaration_opt = new NonTerminal("method_declaration_opt");



    NonTerminal property_declaration = new NonTerminal("property_declaration");
    NonTerminal indexer_declaration = new NonTerminal("indexer_declaration");
    NonTerminal operator_declaration = new NonTerminal("operator_declaration");
    NonTerminal field_declaration = new NonTerminal("field_declaration");

    NonTerminal constant_declarators = new NonTerminal("constant_declarators");
    NonTerminal constant_declarator = new NonTerminal("constant_declarator");



    NonTerminal body = new NonTerminal("body");
    NonTerminal right_arrow = new NonTerminal("right_arrow");


    NonTerminal expression = new NonTerminal("expression");
    NonTerminal assignment = new NonTerminal("assignment");
    NonTerminal non_assignment_expression = new NonTerminal("non_assignment_expression");
    NonTerminal lambda_expression = new NonTerminal("lambda_expression");
    NonTerminal query_expression = new NonTerminal("query_expression");
    NonTerminal conditional_expression = new NonTerminal("conditional_expression");
    NonTerminal unary_expression = new NonTerminal("unary_expression");
    NonTerminal assignment_operator = new NonTerminal("assignment_operator");
    NonTerminal null_coalescing_expression = new NonTerminal("null_coalescing_expression");
    NonTerminal conditional_or_expression = new NonTerminal("conditional_or_expression");
    NonTerminal conditional_and_expression = new NonTerminal("conditional_and_expression");
    NonTerminal inclusive_or_expression = new NonTerminal("inclusive_or_expression");
    NonTerminal exclusive_or_expression = new NonTerminal("exclusive_or_expression");
    NonTerminal and_expression = new NonTerminal("and_expression");
    NonTerminal equality_expression = new NonTerminal("equality_expression");
    NonTerminal relational_expression = new NonTerminal("relational_expression");
    NonTerminal shift_expression = new NonTerminal("shift_expression");
    NonTerminal additive_expression = new NonTerminal("additive_expression");
    NonTerminal multiplicative_expression = new NonTerminal("multiplicative_expression");
    NonTerminal primary_expression = new NonTerminal("primary_expression");
    NonTerminal primary_expression_start = new NonTerminal("primary_expression_start");
    NonTerminal primary_expression_end = new NonTerminal("primary_expression_end");
    NonTerminal primary_expression_ends = new NonTerminal("primary_expression_ends");
    NonTerminal bracket_expression = new NonTerminal("bracket_expression");
    NonTerminal bracket_expressions = new NonTerminal("bracket_expressions");

    NonTerminal member_access = new NonTerminal("member_access");
    NonTerminal method_invocation = new NonTerminal("method_invocation");
    NonTerminal unsafe_access = new NonTerminal("unsafe_access");


    NonTerminal literalExpression = new NonTerminal("literalExpression");
    NonTerminal simpleNameExpression = new NonTerminal("simpleNameExpression");
    NonTerminal parenthesisExpressions = new NonTerminal("parenthesisExpressions");
    NonTerminal memberAccessExpression = new NonTerminal("memberAccessExpression");
    NonTerminal literalAccessExpression = new NonTerminal("literalAccessExpression");
    NonTerminal thisReferenceExpression = new NonTerminal("thisReferenceExpression");
    NonTerminal baseAccessExpression = new NonTerminal("baseAccessExpression");
    NonTerminal objectCreationExpression = new NonTerminal("objectCreationExpression");
    NonTerminal typeofExpression = new NonTerminal("typeofExpression");
    NonTerminal checkedExpression = new NonTerminal("checkedExpression");
    NonTerminal uncheckedExpression = new NonTerminal("uncheckedExpression");
    NonTerminal defaultValueExpression = new NonTerminal("defaultValueExpression");
    NonTerminal anonymousMethodExpression = new NonTerminal("anonymousMethodExpression");
    NonTerminal sizeofExpression = new NonTerminal("sizeofExpression");
    NonTerminal nameofExpression = new NonTerminal("nameofExpression");
    NonTerminal rankarraryExpression = new NonTerminal("rankarraryExpression");
    NonTerminal array_initializer = new NonTerminal("array_initializer");
    NonTerminal anonymousobjExpression = new NonTerminal("anonymousobjExpression");
    NonTerminal new_type0 = new NonTerminal("new_type0");
    NonTerminal new_type1 = new NonTerminal("new_type1");
    NonTerminal new_type2 = new NonTerminal("new_type2");
    NonTerminal new_type3 = new NonTerminal("new_type3");
    NonTerminal expression_list = new NonTerminal("expression_list");




    NonTerminal type_argument_list_opt = new NonTerminal("type_argument_list_opt");
    NonTerminal argument_list = new NonTerminal("argument_list");
    NonTerminal argument = new NonTerminal("argument");
    NonTerminal argument_list_opt = new NonTerminal("argument_list_opt");



    NonTerminal attributes = new NonTerminal("attributes");



    NonTerminal identifier_colon_opt = new NonTerminal("identifier_colon_opt");

    NonTerminal refout_opt = new NonTerminal("refout_opt");
    NonTerminal vartype_opt = new NonTerminal("vartype_opt");

    NonTerminal refout = new NonTerminal("refout");
    NonTerminal vartype = new NonTerminal("vartype");


    NonTerminal qmark_exp_colon_exp = new NonTerminal("qmark_exp_colon_exp");
    NonTerminal qmark_exp_colon_exp_opt = new NonTerminal("qmark_exp_colon_exp_opt");

    NonTerminal qmark_null_coal = new NonTerminal("qmark_null_coal");
    NonTerminal qmark_null_coal_opt = new NonTerminal("qmark_null_coal_opt");


    NonTerminal conditional_and_expressions = new NonTerminal("conditional_and_expressions");

    NonTerminal unary_expressions = new NonTerminal("unary_expressions");
    NonTerminal additive_expressions = new NonTerminal("additive_expressions");
    NonTerminal multiplicative_expressions = new NonTerminal("multiplicative_expressions");
    NonTerminal relational_expressions = new NonTerminal("relational_expressions");
    NonTerminal equality_expressions = new NonTerminal("equality_expressions");
    NonTerminal and_expressions = new NonTerminal("and_expressions");
    NonTerminal exclusive_or_expressions = new NonTerminal("exclusive_or_expressions");
    NonTerminal inclusive_or_expressions = new NonTerminal("inclusive_or_expressions");


    NonTerminal indexer_arguments = new NonTerminal("indexer_arguments");
    NonTerminal indexer_argument = new NonTerminal("indexer_argument");
    NonTerminal anonymous_object_initializer = new NonTerminal("anonymous_object_initializer");
    NonTerminal attribute_arguments_opt = new NonTerminal("attribute_arguments_opt");
    NonTerminal attribute_arguments = new NonTerminal("attribute_arguments");
    NonTerminal attribute_argument = new NonTerminal("attribute_argument");


    NonTerminal object_or_collection_initializer = new NonTerminal("object_or_collection_initializer");
    NonTerminal object_initializer = new NonTerminal("object_initializer");
    NonTerminal collection_initializer = new NonTerminal("collection_initializer");
    NonTerminal member_initializer_list = new NonTerminal("member_initializer_list");
    NonTerminal member_initializer_list_opt = new NonTerminal("member_initializer_list_opt");

    NonTerminal member_initializer = new NonTerminal("member_initializer");


    NonTerminal initializer_value = new NonTerminal("initializer_value");
    NonTerminal element_initializer = new NonTerminal("element_initializer");
    NonTerminal element_initializers = new NonTerminal("element_initializers");

    NonTerminal member_declarator_list = new NonTerminal("member_declarator_list");
    NonTerminal member_declarator = new NonTerminal("member_declarator");
    NonTerminal isType = new NonTerminal("isType");
    NonTerminal base_type_exp = new NonTerminal("base_type_exp");
    NonTerminal anonymous_function_signature = new NonTerminal("anonymous_function_signature");
    NonTerminal explicit_anonymous_function_parameter_list = new NonTerminal("explicit_anonymous_function_parameter_list");
    NonTerminal implicit_anonymous_function_parameter_list = new NonTerminal("implicit_anonymous_function_parameter_list");

    NonTerminal explicit_anonymous_function_parameter = new NonTerminal("explicit_anonymous_function_parameter");

    NonTerminal anonymous_function_body = new NonTerminal("anonymous_function_body");
    NonTerminal from_clause = new NonTerminal("from_clause");
    NonTerminal query_body = new NonTerminal("query_body");


    NonTerminal type_opt = new NonTerminal("type_opt");

    NonTerminal query_body_clause = new NonTerminal("query_body_clause");

    NonTerminal query_body_clauses = new NonTerminal("query_body_clauses");

    NonTerminal select_or_group_clause = new NonTerminal("select_or_group_clause");
    NonTerminal query_continuation = new NonTerminal("query_continuation");
    NonTerminal query_continuation_opt = new NonTerminal("query_continuation_opt");
    NonTerminal let_clause = new NonTerminal("let_clause");
    NonTerminal where_clause = new NonTerminal("where_clause");
    NonTerminal combined_join_clause = new NonTerminal("combined_join_clause");
    NonTerminal orderby_clause = new NonTerminal("orderby_clause");
    NonTerminal into_identifier = new NonTerminal("into_identifier");
    NonTerminal into_identifier_opt = new NonTerminal("into_identifier_opt");
    NonTerminal ordering = new NonTerminal("ordering");
    NonTerminal orderings = new NonTerminal("orderings");

    NonTerminal dir = new NonTerminal("dir");
    NonTerminal dir_opt = new NonTerminal("dir_opt");


    NonTerminal statement = new NonTerminal("statement");
    NonTerminal labeledStatement = new NonTerminal("labeledStatement");
    NonTerminal declarationStatement = new NonTerminal("declarationStatement");
    NonTerminal embeddedStatement = new NonTerminal("embeddedStatement");
    NonTerminal local_variable_declaration = new NonTerminal("local_variable_declaration");
    NonTerminal local_constant_declaration = new NonTerminal("local_constant_declaration");
    NonTerminal block = new NonTerminal("block");
    NonTerminal simple_embedded_statement = new NonTerminal("simple_embedded_statement");
    NonTerminal emptyStatement = new NonTerminal("emptyStatement");
    NonTerminal expressionStatement = new NonTerminal("expressionStatement");
    NonTerminal ifStatement = new NonTerminal("ifStatement");
    NonTerminal switchStatement = new NonTerminal("switchStatement");
    NonTerminal whileStatement = new NonTerminal("whileStatement");
    NonTerminal doStatement = new NonTerminal("doStatement");
    NonTerminal forStatement = new NonTerminal("forStatement");
    NonTerminal foreachStatement = new NonTerminal("foreachStatement");
    NonTerminal breakStatement = new NonTerminal("breakStatement");
    NonTerminal continueStatement = new NonTerminal("continueStatement");
    NonTerminal gotoStatement = new NonTerminal("gotoStatement");
    NonTerminal returnStatement = new NonTerminal("returnStatement");
    NonTerminal throwStatement = new NonTerminal("throwStatement");
    NonTerminal tryStatement = new NonTerminal("tryStatement");
    NonTerminal checkedStatement = new NonTerminal("checkedStatement");
    NonTerminal uncheckedStatement = new NonTerminal("uncheckedStatement");
    NonTerminal lockStatement = new NonTerminal("lockStatement");
    NonTerminal usingStatement = new NonTerminal("usingStatement");
    NonTerminal yieldStatement = new NonTerminal("yieldStatement");
    NonTerminal unsafeStatement = new NonTerminal("unsafeStatement");
    NonTerminal fixedStatement = new NonTerminal("fixedStatement");
    NonTerminal expression_opt = new NonTerminal("expression_opt");


    NonTerminal if_body = new NonTerminal("if_body");
    NonTerminal else_if_body_opt = new NonTerminal("else_if_body_opt");
    NonTerminal switch_section = new NonTerminal("switch_section");
    NonTerminal switch_sections = new NonTerminal("switch_sections");
    NonTerminal for_initializer = new NonTerminal("for_initializer");
    NonTerminal for_initializer_opt = new NonTerminal("for_initializer_opt");
    NonTerminal for_iterator = new NonTerminal("for_iterator");
    NonTerminal for_iterator_opt = new NonTerminal("for_iterator_opt");

    NonTerminal local_variable_type = new NonTerminal("local_variable_type");

    NonTerminal catch_clauses = new NonTerminal("catch_clauses");
    NonTerminal finally_clause = new NonTerminal("finally_clause");
    NonTerminal finally_clause_opt = new NonTerminal("finally_clause_opt");
    NonTerminal resource_acquisition = new NonTerminal("resource_acquisition");
    NonTerminal pointer_type = new NonTerminal("pointer_type");
    NonTerminal fixed_pointer_declarators = new NonTerminal("fixed_pointer_declarators");
    NonTerminal switch_label = new NonTerminal("switch_label");


    NonTerminal statement_list = new NonTerminal("statement_list");

    NonTerminal expressions_plus = new NonTerminal("expressions_plus");

    NonTerminal specific_catch_clause = new NonTerminal("specific_catch_clause");
    NonTerminal general_catch_clause = new NonTerminal("general_catch_clause");
    NonTerminal specific_catch_clauses = new NonTerminal("specific_catch_clauses");
    NonTerminal general_catch_clause_opt = new NonTerminal("general_catch_clause_opt");
    NonTerminal exception_filter = new NonTerminal("exception_filter");
    NonTerminal exception_filter_opt = new NonTerminal("exception_filter_opt");





    NonTerminal enum_base_opt = new NonTerminal("enum_base_opt");
    NonTerminal enum_body = new NonTerminal("enum_body");
    NonTerminal struct_interfaces_opt = new NonTerminal("struct_interfaces_opt");
    NonTerminal object_creation_expression = new NonTerminal("object_creation_expression");
    NonTerminal object_or_collection_initializer_opt = new NonTerminal("object_or_collection_initializer_opt");
    NonTerminal ASSIGNMENT_expression_opt = new NonTerminal("ASSIGNMENT_expression_opt");
    NonTerminal arg_declaration = new NonTerminal("arg_declaration");
    NonTerminal arg_declarations = new NonTerminal("arg_declarations");
    NonTerminal overloadable_operator = new NonTerminal("overloadable_operator");
    NonTerminal type_c_id = new NonTerminal("type_c_id");
    NonTerminal type_c_ids = new NonTerminal("type_c_ids");
    NonTerminal method_member_name = new NonTerminal("method_member_name");
    NonTerminal method_body = new NonTerminal("method_body");
    NonTerminal formal_parameter_list_opt = new NonTerminal("formal_parameter_list_opt");
    NonTerminal formal_parameter_list = new NonTerminal("formal_parameter_list");
    NonTerminal accessor_declarations = new NonTerminal("accessor_declarations");
    NonTerminal variable_declarators = new NonTerminal("variable_declarators");
    NonTerminal accessor_modifier = new NonTerminal("accessor_modifier");
    NonTerminal accessor_body = new NonTerminal("accessor_body");
    NonTerminal set_accessor_declaration = new NonTerminal("set_accessor_declaration");
    NonTerminal get_accessor_declaration = new NonTerminal("get_accessor_declaration");
    NonTerminal event_accessor_declarations = new NonTerminal("event_accessor_declarations");
    NonTerminal add_accessor_declaration = new NonTerminal("add_accessor_declaration");
    NonTerminal remove_accessor_declaration = new NonTerminal("remove_accessor_declaration");
    NonTerminal struct_interfaces = new NonTerminal("struct_interfaces");
    NonTerminal interface_type_list = new NonTerminal("interface_type_list");
    NonTerminal struct_member_declaration = new NonTerminal("struct_member_declaration");
    NonTerminal struct_member_declaration_c1 = new NonTerminal("struct_member_declaration_c1");
    NonTerminal fixed_size_buffer_declarator = new NonTerminal("fixed_size_buffer_declarator");


    NonTerminal struct_body = new NonTerminal("struct_body");









    //place
    NonTerminal literal = new NonTerminal("literal");


    /// <summary>
    /// 
    /// </summary>
    #endregion
    public CSharpGrammar()
    {

      NonGrammarTerminals.Add(SingleLineComment);
      NonGrammarTerminals.Add(DelimitedComment);
      NonGrammarTerminals.Add(ppInstruction);

      //Symbols
      colon = ToTerm(":", "colon");
      semi = ToTerm(";", "semi");
      semi_opt.Rule = (Empty | PreferShiftHere() + semi);
      dot = ToTerm(".", "dot");
      comma = ToTerm(",", "comma");
      comma_opt.Rule = Empty | comma;
      commas_opt.Rule = MakeStarRule(commas_opt, null, comma);
      qmark = ToTerm("?", "qmark");
      qmark_opt.Rule = Empty | PreferShiftHere() + qmark;
      Lbr = ToTerm("{");
      Rbr = ToTerm("}");
      Lpar = ToTerm("(");
      Rpar = ToTerm(")");

      Lparx = ToTerm("(*");






      #region lex
      ABSTRACT = ToTerm("abstract", "ABSTRACT");
      ADD = ToTerm("add", "ADD");
      ALIAS = ToTerm("alias", "ALIAS");
      ARGLIST = ToTerm("__arglist", "ARGLIST");
      AS = ToTerm("as", "AS");
      ASCENDING = ToTerm("ascending", "ASCENDING");
      ASYNC = ToTerm("async", "ASYNC");
      AWAIT = ToTerm("await", "AWAIT");
      BASE = ToTerm("base", "BASE");
      BOOL = ToTerm("bool", "BOOL");
      BREAK = ToTerm("break", "BREAK");
      BY = ToTerm("by", "BY");
      BYTE = ToTerm("byte", "BYTE");
      CASE = ToTerm("case", "CASE");
      CATCH = ToTerm("catch", "CATCH");
      CHAR = ToTerm("char", "CHAR");
      CHECKED = ToTerm("checked", "CHECKED");
      CLASS = ToTerm("class", "CLASS");
      CONST = ToTerm("const", "CONST");
      CONTINUE = ToTerm("continue", "CONTINUE");
      DECIMAL = ToTerm("decimal", "DECIMAL");
      DEFAULT = ToTerm("default", "DEFAULT");
      DELEGATE = ToTerm("delegate", "DELEGATE");
      DESCENDING = ToTerm("descending", "DESCENDING");
      DO = ToTerm("do", "DO");
      DOUBLE = ToTerm("double", "DOUBLE");
      DYNAMIC = ToTerm("dynamic", "DYNAMIC");
      ELSE = ToTerm("else", "ELSE");
      ENUM = ToTerm("enum", "ENUM");
      EQUALS = ToTerm("equals", "EQUALS");
      EVENT = ToTerm("event", "EVENT");
      EXPLICIT = ToTerm("explicit", "EXPLICIT");
      EXTERN = ToTerm("extern", "EXTERN");
      FALSE = ToTerm("false", "FALSE");
      FINALLY = ToTerm("finally", "FINALLY");
      FIXED = ToTerm("fixed", "FIXED");
      FLOAT = ToTerm("float", "FLOAT");
      FOR = ToTerm("for", "FOR");
      FOREACH = ToTerm("foreach", "FOREACH");
      FROM = ToTerm("from", "FROM");
      GET = ToTerm("get", "GET");
      GOTO = ToTerm("goto", "GOTO");
      GROUP = ToTerm("group", "GROUP");
      IF = ToTerm("if", "IF");
      IMPLICIT = ToTerm("implicit", "IMPLICIT");
      IN = ToTerm("in", "IN");
      INT = ToTerm("int", "INT");
      INTERFACE = ToTerm("interface", "INTERFACE");
      INTERNAL = ToTerm("internal", "INTERNAL");
      INTO = ToTerm("into", "INTO");
      IS = ToTerm("is", "IS");
      JOIN = ToTerm("join", "JOIN");
      LET = ToTerm("let", "LET");
      LOCK = ToTerm("lock", "LOCK");
      LONG = ToTerm("long", "LONG");
      NAMEOF = ToTerm("nameof", "NAMEOF");
      NAMESPACE = ToTerm("namespace", "NAMESPACE");
      NEW = ToTerm("new", "NEW");
      NULL = ToTerm("null", "NULL");
      OBJECT = ToTerm("object", "OBJECT");
      ON = ToTerm("on", "ON");
      OPERATOR = ToTerm("operator", "OPERATOR");
      ORDERBY = ToTerm("orderby", "ORDERBY");
      OUT = ToTerm("out", "OUT");
      OVERRIDE = ToTerm("override", "OVERRIDE");
      PARAMS = ToTerm("params", "PARAMS");
      PARTIAL = ToTerm("partial", "PARTIAL");
      PRIVATE = ToTerm("private", "PRIVATE");
      PROTECTED = ToTerm("protected", "PROTECTED");
      PUBLIC = ToTerm("public", "PUBLIC");
      READONLY = ToTerm("readonly", "READONLY");
      REF = ToTerm("ref", "REF");
      REMOVE = ToTerm("remove", "REMOVE");
      RETURN = ToTerm("return", "RETURN");
      SBYTE = ToTerm("sbyte", "SBYTE");
      SEALED = ToTerm("sealed", "SEALED");
      SELECT = ToTerm("select", "SELECT");
      SET = ToTerm("set", "SET");
      SHORT = ToTerm("short", "SHORT");
      SIZEOF = ToTerm("sizeof", "SIZEOF");
      STACKALLOC = ToTerm("stackalloc", "STACKALLOC");
      STATIC = ToTerm("static", "STATIC");
      STRING = ToTerm("string", "STRING");
      STRUCT = ToTerm("struct", "STRUCT");
      SWITCH = ToTerm("switch", "SWITCH");
      THIS = ToTerm("this", "THIS");
      THROW = ToTerm("throw", "THROW");
      TRUE = ToTerm("true", "TRUE");
      TRY = ToTerm("try", "TRY");
      TYPEOF = ToTerm("typeof", "TYPEOF");
      UINT = ToTerm("uint", "UINT");
      ULONG = ToTerm("ulong", "ULONG");
      UNCHECKED = ToTerm("unchecked", "UNCHECKED");
      UNSAFE = ToTerm("unsafe", "UNSAFE");
      USHORT = ToTerm("ushort", "USHORT");
      USING = ToTerm("using", "USING");
      VAR = ToTerm("		var", "VAR");
      VIRTUAL = ToTerm("virtual", "VIRTUAL");
      VOID = ToTerm("void", "VOID");
      VOLATILE = ToTerm("volatile", "VOLATILE");
      WHEN = ToTerm("when", "WHEN");
      WHERE = ToTerm("where", "WHERE");
      WHILE = ToTerm("while", "WHILE");
      YIELD = ToTerm("yield", "YIELD");
      #endregion






      Root = compilation_unit;

      compilation_unit.Rule = extern_alias_directives_opt + using_directives_opt + global_attribute_section_opt
     /* +  namespace_member_declarations*/;


      extern_alias_directive.Rule = PreferShiftHere() + ToTerm("extern") + "alias" + identifier + semi;
      extern_alias_directives_opt.Rule = MakeStarRule(extern_alias_directives_opt, null, extern_alias_directive);

      using_directive.Rule = using_alias_directive | using_ns_directive | using_st_directive;
      using_directives.Rule = MakePlusRule(using_directives, null, using_directive);
      using_directives_opt.Rule = Empty | using_directives;

      using_alias_directive.Rule = "using" + identifier + "=" + namespace_or_type_name + semi;
      using_ns_directive.Rule = "using" + namespace_or_type_name + semi;
      using_st_directive.Rule = ToTerm("using") + ToTerm("static") + namespace_or_type_name + semi;

      global_attribute_section_opt.Rule = MakeStarRule(global_attribute_section_opt, global_attribute_section);

      global_attribute_section.Rule = PreferShiftHere() + "[" + global_attribute_target + ":" + attribute_list + comma_opt + "]";


      attribute_list.Rule = MakePlusRule(attribute_list, comma, attribute);

      global_attribute_target.Rule = identifier;

      var attribute_arguments_opt_opt = new NonTerminal("attribute_arguments_opt_opt");
      attribute.Rule = namespace_or_type_name + attribute_arguments_opt_opt;
      attribute_arguments_opt_opt.Rule = attribute_arguments_opt | Empty;
      attribute_arguments_opt.Rule = "(" + attribute_arguments + ")";
      attribute_arguments.Rule = MakeStarRule(attribute_arguments, ToTerm(","), attribute_argument);

      attribute_argument.Rule = ((PreferShiftHere()+ identifier + ":") | Empty) + expression;


      ///成员
      ///
      namespace_member_declarations_opt.Rule = MakeStarRule(namespace_member_declarations_opt, namespace_member_declaration);


      namespace_member_declaration.Rule = namespace_declaration | type_declaration;


      namespace_declaration.Rule = "namespace" + qualified_identifier + namespace_body + semi_opt;


      namespace_body.Rule = "{" + extern_alias_directives_opt + using_directives_opt + namespace_member_declarations_opt + "}";


      qualified_identifier.Rule = MakePlusRule(qualified_identifier, dot, identifier);

      #region mod
      //修饰符
      all_member_modifiers.Rule = MakeStarRule(all_member_modifiers, all_member_modifier);

      all_member_modifier.Rule = NEW
| PUBLIC
| PROTECTED
| INTERNAL
| PRIVATE
| READONLY
| VOLATILE
| VIRTUAL
| SEALED
| OVERRIDE
| ABSTRACT
| STATIC
| UNSAFE
| EXTERN
| PARTIAL
| ASYNC  // C# 5
;
      #endregion


      //类

      type_declaration.Rule = attributes_opt + all_member_modifiers + (class_definition | struct_definition | interface_definition | enum_definition | delegate_definition);


      class_definition.Rule = CLASS + identifier + type_parameter_list_opt + class_base_opt + type_parameter_constraints_clauses_opt + class_body + semi_opt;
      class_body.Rule = "{" + class_member_declarations_opt + "}";
      class_member_declarations_opt.Rule = MakeStarRule(class_member_declarations_opt, class_member_declaration);
      class_member_declaration.Rule = attributes_opt + all_member_modifiers + (common_member_declaration | destructor_definition);





      struct_definition.Rule = STRUCT + identifier + type_parameter_list_opt + struct_interfaces_opt + type_parameter_constraints_clauses_opt + struct_body + semi_opt;


      enum_definition.Rule = ENUM + identifier + enum_base_opt + enum_body + semi_opt;
      enum_base_opt.Rule = enum_base | Empty;

      //(common_member_declaration | destructor_definition)
      common_member_declaration.Rule =
        constant_declaration
       | typed_member_declaration
       | event_declaration
       | conversion_operator_declarator_opt // C# 6
       | constructor_declaration
       | method_declaration_opt
       | class_definition
       | struct_definition
       | interface_definition
       | enum_definition
       | delegate_definition
       ;


      //typed_member_declaration
      typed_member_declaration.Rule = type + (namespace_or_type_name + "." + indexer_declaration
| method_declaration
| property_declaration
| indexer_declaration
| operator_declaration
| field_declaration
);


      //constant_declarators

      constant_declarators.Rule = MakePlusRule(constant_declarators, ToTerm(","), constant_declarator);

      //constant_declarator

      constant_declarator.Rule = identifier + "=" + expression;

      //exp
      expression.Rule = assignment
| non_assignment_expression
;




      //Type parameters
      type_parameter.Rule = attributes_opt + identifier;
      type_parameters.Rule = MakePlusRule(type_parameters, comma, type_parameter);
      type_parameter_list_opt.Rule = Empty | PreferShiftHere() + "<" /* gen_lt*/ + type_parameters + ">";
      type_parameter_constraints_clause.Rule = "where" + type_parameter + colon + type_parameter_constraints;
      type_parameter_constraints.Rule = MakePlusRule(type_parameter_constraints, comma, type_parameter_constraint);
      type_parameter_constraints_clauses_opt.Rule = MakeStarRule(type_parameter_constraints_clauses_opt, null, type_parameter_constraints_clause);
      //Note for post-processing - make sure the order is correct: new() is always last, etc. See p.503 of the spec 
      type_parameter_constraint.Rule = namespace_or_type_name | "class" | "struct" | ToTerm("new") + Lpar + Rpar;







      gen_lt.Rule = CustomActionHere(this.ResolveLessThanConflict) + "<";
      type_argument_list.Rule = gen_lt + type_list + ">";
      //qual_name_segment.Rule = dot + identifier
      //                 | "::" + identifier
      //                 | type_argument_list;
      //qual_name_segments_opt.Rule = MakeStarRule(qual_name_segments_opt, null, qual_name_segment);
      identifier_or_builtin.Rule = identifier | builtin_type;
      // namespace_or_type_name.Rule =PreferShiftHere()+ identifier + qual_name_segments_opt;
      //type_argument_list.Rule = type_argument_list | Empty;
      var namespace_or_type_name_1 = new NonTerminal("namespace_or_type_name_1");
      namespace_or_type_name_1.Rule = identifier + type_argument_list_opt;
      var namespace_or_type_name_2 = new NonTerminal("namespace_or_type_name_2");
      namespace_or_type_name_2.Rule = "." + identifier + type_argument_list_opt;
         var namespace_or_type_name_2s = new NonTerminal("namespace_or_type_name_2s");
      namespace_or_type_name_2s.Rule = MakeStarRule(namespace_or_type_name_2s, namespace_or_type_name_2);
      var qualified_alias_member = new NonTerminal("qualified_alias_member");
      qualified_alias_member.Rule =PreferShiftHere()+ identifier + "::" + identifier + type_argument_list_opt;

      namespace_or_type_name.Rule =( namespace_or_type_name_1| qualified_alias_member) + namespace_or_type_name_2s;

      builtin_type.Rule = integral_type | "bool" | "decimal" | "float" | "double" | "string" | "object";
      integral_type.Rule = ToTerm("sbyte") | "byte" | "short" | "ushort" | "int" | "uint" | "long" | "ulong" | "char";


      typearg_or_gendimspec_list.Rule = type_argument_list;
      generic_dimension_specifier_opt.Rule = Empty | gen_lt + commas_opt + ">";

      type.Rule = type_or_void + qmark_opt + rank_specifiers_opt + typearg_or_gendimspec_list;
      type_list.Rule = MakePlusRule(type_list, comma, type);
      type_or_void.Rule = namespace_or_type_name|PreferShiftHere()+ "void";

      comma_list_opt.Rule = MakeStarRule(comma_list_opt, comma);
      rank_specifier.Rule = "[" + comma_list_opt + "]";
      rank_specifiers.Rule = MakePlusRule(rank_specifiers, null, rank_specifier);
      rank_specifiers_opt.Rule = rank_specifiers.Q();



      //type_argument_list_opt
      type_argument_list_opt.Rule = Empty | type_argument_list;

      //member_access
      member_access.Rule = qmark_opt + "." + identifier + type_argument_list_opt;
      //method_invocation
      method_invocation.Rule
= "(" + argument_list_opt + ")"
;



      //argument_list_opt
      argument_list_opt.Rule = (Empty | argument_list);

      argument_list.Rule = MakePlusRule(argument_list, ToTerm(","), argument);

      argument.Rule = identifier_colon_opt + (refout_opt) + (vartype_opt) + expression;


      //refout_opt
      refout.Rule = REF | OUT;
      refout_opt.Rule = (Empty | refout);
      //vartype_opt
      vartype.Rule = VAR | type;
      vartype_opt.Rule = Empty | vartype;




      //identifier_colon_opt ->argument_name
      identifier_colon_opt.Rule = identifier + (Empty | colon);


      //assignment
      assignment.Rule = unary_expression + assignment_operator + expression;

      assignment_operator.Rule =
ToTerm("=") | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
;

      //non_assignment_expression
      non_assignment_expression.Rule = lambda_expression | query_expression | conditional_expression;

      lambda_expression.Rule = anonymous_function_signature + right_arrow + anonymous_function_body;

      //conditional_expression 

      //conditional_expression
      // : null_coalescing_expression
      // | null_coalescing_expression '?' expression ':' expression
      // ;
      conditional_expression.Rule = null_coalescing_expression + qmark_exp_colon_exp_opt;

      qmark_exp_colon_exp_opt.Rule = qmark_exp_colon_exp | Empty;

      qmark_exp_colon_exp.Rule = qmark + expression + ":" + expression;


      //null_coalescing_expression

      null_coalescing_expression.Rule = conditional_or_expression;

      qmark_null_coal.Rule = "??" + null_coalescing_expression;
      qmark_null_coal_opt.Rule = (Empty | qmark_null_coal);



      //conditional_and_expression


      conditional_or_expression.Rule = conditional_and_expressions;

      //conditional_and_expression

      conditional_and_expressions.Rule = MakePlusRule(conditional_and_expressions, ToTerm("||"), conditional_and_expression);

      conditional_and_expressions.Rule = inclusive_or_expressions;

      //inclusive_or_expression
      inclusive_or_expressions.Rule = MakePlusRule(inclusive_or_expressions, ToTerm("&&"), inclusive_or_expression);

      inclusive_or_expression.Rule = exclusive_or_expressions;
      //exclusive_or_expression

      exclusive_or_expressions.Rule = MakePlusRule(exclusive_or_expressions, ToTerm("|"), exclusive_or_expression);

      exclusive_or_expression.Rule = and_expressions;

      //
      and_expressions.Rule = MakePlusRule(and_expressions, ToTerm("^"), and_expression);

      and_expression.Rule = equality_expressions;

      equality_expressions.Rule = MakePlusRule(equality_expressions, ToTerm("&"), equality_expression);

      equality_expression.Rule = relational_expressions;

      relational_expressions.Rule = MakePlusRule(relational_expressions, ToTerm("==") | "!=", relational_expression);


      var ss_exp = new NonTerminal("ss_exp");
      var ss_exps = new NonTerminal("ss_exps");

      ss_exp.Rule = (ToTerm("<") | ">" | "<=" | ">=") + shift_expression | IS + namespace_or_type_name | AS + type;
      ss_exps.Rule = MakeStarRule(ss_exps, ss_exp);


      relational_expression.Rule = shift_expression + ss_exps;
      // shift_expression_more;


      shift_expression.Rule = additive_expressions;

      additive_expressions.Rule = MakePlusRule(additive_expressions, ToTerm("<<") | ">>", additive_expression);

      additive_expression.Rule = multiplicative_expressions;

      multiplicative_expressions.Rule = MakePlusRule(multiplicative_expressions, ToTerm("+") | "-", multiplicative_expression);

      multiplicative_expression.Rule = unary_expressions;

      unary_expressions.Rule = MakePlusRule(unary_expressions, ToTerm("*") | "/" | "%", unary_expression);

      var op_op = new NonTerminal("op_op");
      op_op.Rule = ToTerm("+") | "-" | "!" | "~" | "++" | "--" | AWAIT | "&" | "*";

      unary_expression.Rule = primary_expression | op_op + unary_expression
          | "(" + type + ")" + unary_expression;


      primary_expression.Rule = primary_expression_start + bracket_expressions + primary_expression_ends;

      bracket_expressions.Rule = MakeStarRule(bracket_expressions, bracket_expression);

      unsafe_access.Rule = "->" + identifier;

      primary_expression_end.Rule = (member_access | method_invocation | "++" | "--" | unsafe_access) + bracket_expressions;

      primary_expression_ends.Rule = MakeStarRule(primary_expression_ends, primary_expression_end);

      primary_expression_start.Rule = literalExpression
          | simpleNameExpression | parenthesisExpressions | memberAccessExpression 
          //| thisReferenceExpression | baseAccessExpression
          //| objectCreationExpression
          | typeofExpression
          | checkedExpression
          | uncheckedExpression
          | defaultValueExpression
          //| anonymousMethodExpression
          | sizeofExpression
          | nameofExpression;

      objectCreationExpression.Rule = "new" + (new_type0 | anonymousobjExpression | rankarraryExpression);

      rankarraryExpression.Rule = rank_specifier + array_initializer;

      anonymousobjExpression.Rule = anonymous_object_initializer;

      literal.Rule = Number | StringLiteral | CharLiteral | "true" | "false" | "null";

      literalExpression.Rule = literal;

      thisReferenceExpression.Rule = THIS;


      literalAccessExpression.Rule = Number + "." + identifier;

      var identifiers = new NonTerminal("identifiers");

      identifiers.Rule = MakePlusRule(identifiers, ToTerm("."), identifier);

      nameofExpression.Rule = NAMEOF + "(" + identifiers + ")";

      sizeofExpression.Rule = SIZEOF + "(" + type + ")";

      defaultValueExpression.Rule =PreferShiftHere()+ DEFAULT + "(" + type + ")";

      uncheckedExpression.Rule = UNCHECKED + "(" + expression + ")";

      checkedExpression.Rule = CHECKED + "(" + expression + ")";

      typeofExpression.Rule = "typeof" + Lpar + type + Rpar;


      var predefined_type = new NonTerminal("predefined_type");
     // qualified_alias_member.Rule = identifier + "::" + identifier + type_argument_list_opt;
      predefined_type.Rule = BOOL | BYTE | CHAR | DECIMAL | DOUBLE | FLOAT | INT | LONG
| OBJECT | SBYTE | SHORT | STRING | UINT | ULONG | USHORT
;
      memberAccessExpression.Rule = qualified_alias_member | predefined_type;

      simpleNameExpression.Rule = identifier + type_argument_list_opt;

      parenthesisExpressions.Rule = "(" + expression + ")";



      indexer_arguments.Rule = MakePlusRule(indexer_arguments, ToTerm(","), indexer_argument);
      indexer_argument.Rule = identifier_colon_opt + expression;

      bracket_expression.Rule =qmark_opt + "[" + indexer_arguments + "]";
      expression_list.Rule = MakePlusRule(expression_list, ToTerm(","), expression);




      object_creation_expression.Rule = "(" + argument_list_opt + ")" + object_or_collection_initializer_opt;
      object_or_collection_initializer_opt.Rule = object_or_collection_initializer | Empty;
      object_or_collection_initializer.Rule = object_initializer | collection_initializer;

      object_initializer.Rule = "{" + member_initializer_list_opt + "}";
      member_initializer_list_opt.Rule = member_initializer_list + comma_opt;
      member_initializer_list.Rule = MakePlusRule(member_initializer_list, comma, member_initializer);


      ASSIGNMENT_expression_opt.Rule = ("=" + expression) | Empty;
      arg_declaration.Rule = type + identifier + ASSIGNMENT_expression_opt;


      arg_declarations.Rule = MakePlusRule(arg_declarations, ToTerm(","), arg_declaration);
      operator_declaration.Rule = OPERATOR + overloadable_operator + "(" + arg_declarations + ")" + (body | right_arrow + expression + ";");


      type_c_id.Rule = type_argument_list_opt + "." + identifier;
      type_c_ids.Rule = MakeStarRule(type_c_ids, type_c_id);

      method_member_name.Rule = (identifier | identifier + "::" + identifier) + type_c_ids;


      method_declaration.Rule = method_member_name + type_parameter_list_opt + "(" + formal_parameter_list_opt + ")"
          + type_parameter_constraints_clauses_opt + (method_body | right_arrow + expression + ";")

          ;
      formal_parameter_list_opt.Rule = formal_parameter_list | Empty;
      parameter_array_comma.Rule = "," + parameter_array;
      parameter_array_opt.Rule = parameter_array_comma | Empty;

      formal_parameter_list.Rule = parameter_array | fixed_parameters + parameter_array_opt;
      destructor_definition.Rule = "~" + identifier + "(" + ")" + body;

      indexer_declaration.Rule = THIS + "[" + formal_parameter_list + "]" + ("(" + accessor_declarations + ")" | right_arrow + expression + ";");


      constant_declaration.Rule = CONST + type + constant_declarators + ";";

      property_declaration.Rule = namespace_or_type_name + ("(" + accessor_declarations + ")" + property_declaration_value_opt | property_declaration_read_value);
      property_declaration_value.Rule = "=" + variable_initializer + ";";
      property_declaration_value_opt.Rule = property_declaration_value | Empty;
      property_declaration_read_value.Rule = right_arrow + expression + ";";

      field_declaration.Rule
= variable_declarators + ";";


      variable_declarators.Rule = MakePlusRule(variable_declarators, ToTerm(","), variable_declarator);

      p_variable_initializer.Rule = "=" + variable_initializer;
      p_variable_initializer_opt.Rule = p_variable_initializer | Empty;
      variable_declarator.Rule = identifier + p_variable_initializer_opt;

      variable_initializer.Rule = expression | array_initializer;

      variable_initializers.Rule = MakePlusRule(variable_initializers, ToTerm(","), variable_initializer);

      variable_initializers_opt.Rule = variable_initializers + comma_opt;
      variable_initializers_opt_opt.Rule = variable_initializers_opt | Empty;
      array_initializer.Rule = "{" + variable_initializers_opt_opt + "}";

      parameter_array.Rule = attributes_opt + PARAMS + type + identifier;

      enum_base.Rule = ":" + type;

      enum_member_declaration.Rule = "{" + enum_member_declarations_opt_opt + "}";

      enum_member_declarations_opt_opt.Rule = enum_member_declarations_opt | Empty;

      enum_member_declarations_opt.Rule = enum_member_declarations + comma_opt;

      enum_member_declarations.Rule = MakePlusRule(enum_member_declarations, ToTerm(","), enum_member_declaration);


      enum_member_declaration.Rule = attributes_opt + identifier + expression_get_opt;
      expression_get_opt.Rule = expression_get | Empty;
      expression_get.Rule = "=" + expression;

      pointer_type.Rule = type + "*";
      fixed_pointer_declarators.Rule = MakePlusRule(fixed_pointer_declarators, ToTerm(","), fixed_pointer_declarator);
      fixed_pointer_declarator.Rule = identifier + "=" + fixed_pointer_initializer;
      var ma1 = new NonTerminal("ma1", PreferShiftHere() + "&");
      fixed_pointer_declarator.Rule = ma1.Q() + expression | local_variable_initializer_unsafe;

      fixed_size_buffer_declarator.Rule = identifier + "[" + expression + "]";
      local_variable_initializer_unsafe.Rule = STACKALLOC + type + "[" + expression + "]";

      right_arrow.Rule = ToTerm("=>");


      ket_expression.Rule = "[" + expression + "]";

      member_initializer.Rule = (identifier | ket_expression) + "=" + initializer_value;

      initializer_value.Rule = expression | object_or_collection_initializer;

      collection_initializer.Rule = "{" + element_initializers + "}";

      element_initializers.Rule = MakePlusRule(element_initializers, comma, element_initializer);

      element_initializer.Rule = non_assignment_expression | "{" + expression_list + "}";

      anonymous_object_initializer.Rule = "{" + member_declarator_list_opt_opt + "}";

      member_declarator_list_opt_opt.Rule = member_declarator_list_opt | Empty;
      member_declarator_list_opt.Rule = member_declarator_list + comma_opt;
      member_declarator_list.Rule = MakePlusRule(member_declarator_list, comma, member_declarator);
      member_declarator.Rule = primary_expression | identifier + "=" + expression;

      anonymous_function_signature.Rule = "(" + ")" | "(" + explicit_anonymous_function_parameter_list + ")"
          | "(" + implicit_anonymous_function_parameter_list + ")"
          | identifier;
      explicit_anonymous_function_parameter.Rule = refout_opt + type + identifier;
      implicit_anonymous_function_parameter_list.Rule = identifiers;

      anonymous_function_body.Rule = expression | block;

      query_expression.Rule = from_clause + query_body;

      from_clause.Rule = FROM + type_opt + identifier + IN + expression;
      query_body.Rule = query_body_clauses + select_or_group_clause + query_continuation_opt;
      query_body_clauses.Rule = MakeStarRule(query_body_clauses, query_body_clause);
      query_body_clause.Rule = from_clause | let_clause | where_clause | combined_join_clause | orderby_clause;
      query_continuation_opt.Rule = query_continuation | Empty;
      let_clause.Rule = LET + identifier + "=" + expression;
      where_clause.Rule = WHERE + expression;

      combined_join_clause.Rule = JOIN + type_opt + identifier + IN + expression + ON + expression + EQUALS + expression + into_identifier_opt;

      into_identifier.Rule = INTO + identifier;
      into_identifier_opt.Rule = into_identifier | Empty;
      orderings.Rule = MakePlusRule(orderings, ToTerm(","), ordering);

      orderby_clause.Rule = ORDERBY + orderings;

      ordering.Rule = expression + dir_opt;

      dir_opt.Rule = dir | Empty;
      dir.Rule = ASCENDING | DESCENDING;

      select_or_group_clause.Rule = SELECT + expression | GROUP + expression + BY + expression;
      query_continuation.Rule =PreferShiftHere()+ INTO + identifier + query_body;


      statement.Rule = labeledStatement | declarationStatement | embeddedStatement;
      labeledStatement.Rule = identifier + ":" + statement;
      declarationStatement.Rule = (local_variable_declaration | local_constant_declaration) + ";";

      embeddedStatement.Rule = block | simple_embedded_statement;

      simple_embedded_statement.Rule = emptyStatement | expressionStatement |
          ifStatement | switchStatement | whileStatement | doStatement |
          forStatement | foreachStatement | breakStatement | continueStatement |
          gotoStatement | returnStatement | throwStatement | tryStatement | checkedStatement
          | uncheckedStatement | lockStatement | usingStatement | yieldStatement | unsafeStatement
          | fixedStatement;

      statement_list_opt.Rule = statement_list | Empty;
      statement_list.Rule = MakePlusRule(statement_list, statement);
      block.Rule = "{" + statement_list_opt + "}";

      local_variable_declarators.Rule = MakePlusRule(local_variable_declarators, comma, local_variable_declarator);

      local_variable_declaration.Rule = local_variable_type + local_variable_declarators;

      local_variable_type.Rule = type | "var";
      local_variable_initializer_get.Rule = "=" + local_variable_initializer;
      local_variable_initializer_get_opt.Rule = local_variable_initializer_get | Empty;

      local_variable_declarator.Rule = identifier + local_variable_initializer_get_opt;

      local_variable_initializer.Rule = expression | array_initializer | local_variable_initializer_unsafe;

      local_constant_declaration.Rule = CONST + type + constant_declarators;

      if_body.Rule = block | simple_embedded_statement;
      switch_labels.Rule = MakePlusRule(switch_labels, switch_label);

      switch_sections.Rule = MakeStarRule(switch_sections, switch_section);
      switch_section.Rule = switch_labels + statement_list;

      switch_label.Rule = CASE + expression + ":" | DEFAULT + ":";
      for_initializer_opt.Rule = for_initializer | Empty;
      for_iterator_opt.Rule = for_iterator | Empty;
      expressions.Rule = MakePlusRule(expressions, comma, expression);
      for_initializer.Rule = local_variable_declaration | expressions;
      for_iterator.Rule = expressions;
      specific_catch_clauses.Rule = MakePlusRule(specific_catch_clauses, specific_catch_clause);
      general_catch_clause_opt.Rule = general_catch_clause | Empty;

      catch_clauses.Rule = specific_catch_clauses + general_catch_clause_opt | general_catch_clause;

      identifier_opt.Rule = identifier | Empty;
      specific_catch_clause.Rule = CATCH + "(" + type + identifier_opt + ")" + exception_filter_opt + block;
      general_catch_clause.Rule = CATCH + exception_filter_opt + block;
      exception_filter.Rule = WHEN + "(" + expression + ")";
      finally_clause_opt.Rule = finally_clause | Empty;
      finally_clause.Rule = FINALLY + block;
      resource_acquisition.Rule = local_variable_declaration | expression;

      event_accessor_declarations_opt.Rule = namespace_or_type_name + "{" + event_accessor_declarations + "}";
      event_declaration_first.Rule = variable_declarators + ";";
      event_declaration.Rule = EVENT + type + (event_declaration_first | event_accessor_declarations_opt);

      constructor_initializer_opt.Rule = constructor_initializer | Empty;

      constructor_declaration.Rule = identifier + "(" + formal_parameter_list_opt + ")" + constructor_initializer_opt + body;

      fixed_parameters.Rule = MakePlusRule(fixed_parameters, comma, fixed_parameter);

      fixed_parameter.Rule = attributes_opt + parameter_modifier_opt + arg_declaration | ARGLIST;

      parameter_modifier_opt.Rule = parameter_modifier | Empty;
      parameter_modifier.Rule = REF | OUT | THIS;

      accessor_declarations.Rule = attributes_opt + accessor_modifier_opt;
      accessor_modifier_opt.Rule = accessor_modifier | Empty;
      accessor_modifier.Rule = PROTECTED
| INTERNAL
| PRIVATE
| PROTECTED + INTERNAL
| INTERNAL + PROTECTED
;
      accessor_body.Rule = block | ";";


      set_accessor_declaration.Rule = attributes_opt + accessor_modifier_opt + SET + accessor_body;
      get_accessor_declaration.Rule = attributes_opt + accessor_modifier_opt + GET + accessor_body;



      variant_type_parameter_list_opt.Rule = variant_type_parameter_list | Empty;
      delegate_definition.Rule = DELEGATE + type + identifier + variant_type_parameter_list_opt + "(" + formal_parameter_list_opt + ")" + type_parameter_constraints_clauses_opt + semi;

      variant_type_parameter_list.Rule = gen_lt + variant_type_parameters + ">";
      variant_type_parameters.Rule = MakePlusRule(variant_type_parameters, ToTerm(","), variant_type_parameter);
      variant_type_parameter.Rule = attributes_opt + variance_annotation_opt + identifier;
      variance_annotation_opt.Rule = variance_annotation | Empty;
      variance_annotation.Rule = IN | OUT;

      interface_definition.Rule = INTERFACE + identifier + variant_type_parameter_list_opt + interface_base_opt + type_parameter_constraints_clauses_opt + interface_body + semi_opt;

      interface_base_opt.Rule = interface_base | Empty;

      interface_base.Rule = ":" + interface_type_list;
      interface_type_list.Rule = MakePlusRule(interface_type_list, comma, namespace_or_type_name);

      interface_body.Rule = "{" + interface_member_declarations + "}";
      interface_member_declarations.Rule = MakeStarRule(interface_member_declarations, interface_member_declaration);

      interface_member_declaration.Rule = attributes_opt + NEW.Q() + (interface_member_declaration3 | interface_member_declaration2 | interface_member_declaration1);

      interface_member_declaration1.Rule = EVENT + type + identifier + ";";
      interface_member_declaration2.Rule = UNSAFE.Q() + VOID + identifier + type_parameter_list_opt + "(" + formal_parameter_list_opt + ")" + type_parameter_constraints_clauses_opt + ";";
      interface_member_declaration3_1.Rule = UNSAFE.Q() + type + identifier + type_parameter_list_opt + "(" + formal_parameter_list_opt + ")" + type_parameter_constraints_clauses_opt + ";";
      interface_member_declaration3_2.Rule = UNSAFE.Q() + type + identifier + "{" + interface_accessors + "}";
      interface_member_declaration3_3.Rule = UNSAFE.Q() + type + THIS + "[" + formal_parameter_list + "]" + "{" + interface_accessors + "}";
      interface_member_declaration3.Rule = interface_member_declaration3_1 | interface_member_declaration3_2 | interface_member_declaration3_3;


      SET_access.Rule = attributes_opt + SET + ";";
      GET_access.Rule = attributes_opt + GET + ";";
      SET_access_opt.Rule = SET_access_opt | Empty;
      GET_access_opt.Rule = GET_access_opt | Empty;


      GET_access_Full.Rule = GET + ";" + SET_access_opt;
      SET_access_Full.Rule = SET + ";" + GET_access_opt;
      SET_or_Get.Rule = GET_access_Full | SET_access_Full;
      interface_accessors.Rule = attributes_opt + SET_or_Get;

      struct_interfaces_opt.Rule = struct_interfaces | Empty;
      struct_interfaces.Rule = ":" + interface_type_list;
      struct_body.Rule = "{" + struct_member_declarations + "}";
      struct_member_declarations.Rule = MakeStarRule(struct_member_declarations, struct_member_declaration);

      struct_member_declaration.Rule = attributes_opt + all_member_modifiers + struct_member_declaration0;
      struct_member_declaration1.Rule = common_member_declaration;
      fixed_size_buffer_declarators.Rule = MakePlusRule(fixed_size_buffer_declarators, fixed_size_buffer_declarator);

      struct_member_declaration2.Rule = FIXED + type + fixed_size_buffer_declarators + ";";
      struct_member_declaration0.Rule = struct_member_declaration1 | struct_member_declaration2;

      //Statement

      emptyStatement.Rule = ";";
      expressionStatement.Rule = expression + ";";
      ifStatement.Rule = IF + "(" + expression + ")" + if_body + else_if_body_opt;
      else_if_body_opt.Rule = (PreferShiftHere()+ELSE + if_body) | Empty;
      switchStatement.Rule = SWITCH + "(" + expression + ")" + "{" + switch_sections + "}";
      whileStatement.Rule = WHILE + "(" + expression + ")" + embeddedStatement;
      doStatement.Rule = DO + embeddedStatement + WHILE + "(" + expression + ")" + ";";
      forStatement.Rule = FOR + "(" + for_initializer_opt + ";" + expression_opt + ";" + for_iterator_opt + ")" + embeddedStatement;

      foreachStatement.Rule = FOREACH + "(" + local_variable_type + identifier + IN + expression + ")" + embeddedStatement;
      breakStatement.Rule = BREAK + ";";
      continueStatement.Rule = CONTINUE + ";";
      gotoStatement.Rule = GOTO + (identifier | CASE + expression | DEFAULT) + ";";
      returnStatement.Rule = RETURN + expression_opt + ";";
      throwStatement.Rule = THROW + expression_opt + ";";
      tryStatement.Rule = TRY + block + (catch_clauses + finally_clause_opt | finally_clause);
      checkedStatement.Rule = CHECKED + block;
      uncheckedStatement.Rule = UNCHECKED + block;
      lockStatement.Rule = LOCK + "(" + expression + ")" + embeddedStatement;
      usingStatement.Rule = USING + "(" + resource_acquisition + ")" + embeddedStatement;
      yieldStatement.Rule = YIELD + (RETURN + expression | BREAK) + ";";
      unsafeStatement.Rule = UNSAFE + block;
      fixedStatement.Rule = FIXED + "(" + pointer_type + fixed_pointer_declarators + ")" + embeddedStatement;

      expression_opt.Rule = expression | Empty;

      explicit_anonymous_function_parameter_list.Rule = MakePlusRule(explicit_anonymous_function_parameter_list, comma, explicit_anonymous_function_parameter);

      exception_filter_opt.Rule = exception_filter | Empty;
      type_opt.Rule = type | Empty;

    }

    TerminalSet _skipTokensInPreview = new TerminalSet(); //used in token preview for conflict resolution
    TerminalSet _skipTokensInPreview2 = new TerminalSet(); //used in token preview for conflict resolution

    NonTerminal fixed_size_buffer_declarators = new NonTerminal("fixed_size_buffer_declarators");
    NonTerminal struct_member_declaration0 = new NonTerminal("struct_member_declaration0");

    NonTerminal struct_member_declaration1 = new NonTerminal("struct_member_declaration1");
    NonTerminal struct_member_declaration2 = new NonTerminal("struct_member_declaration2");

    NonTerminal struct_member_declarations = new NonTerminal("struct_member_declarations");

    NonTerminal SET_or_Get = new NonTerminal("SET_or_Get");

    NonTerminal SET_access_Full = new NonTerminal("SET_access_Full");
    NonTerminal GET_access_Full = new NonTerminal("GET_access_Full");

    NonTerminal SET_access = new NonTerminal("SET_access");
    NonTerminal GET_access = new NonTerminal("GET_access");

    NonTerminal SET_access_opt = new NonTerminal("SET_access_opt");
    NonTerminal GET_access_opt = new NonTerminal("GET_access_opt");
    NonTerminal interface_accessors = new NonTerminal("interface_accessors");

    NonTerminal interface_member_declaration1 = new NonTerminal("interface_member_declaration1");
    NonTerminal interface_member_declaration2 = new NonTerminal("interface_member_declaration2");
    NonTerminal interface_member_declaration3 = new NonTerminal("interface_member_declaration3");
    NonTerminal interface_member_declaration3_1 = new NonTerminal("interface_member_declaration3_1");
    NonTerminal interface_member_declaration3_2 = new NonTerminal("interface_member_declaration3_2");
    NonTerminal interface_member_declaration3_3 = new NonTerminal("interface_member_declaration3_3");

    NonTerminal interface_member_declaration = new NonTerminal("interface_member_declaration");
    NonTerminal interface_member_declarations = new NonTerminal("interface_member_declarations");

    NonTerminal interface_body = new NonTerminal("interface_body");

    NonTerminal interface_base_opt = new NonTerminal("interface_base_opt");

    NonTerminal interface_base = new NonTerminal("interface_base");

    NonTerminal variance_annotation = new NonTerminal("variance_annotation");

    NonTerminal variance_annotation_opt = new NonTerminal("variance_annotation_opt");

    NonTerminal variant_type_parameters = new NonTerminal("variant_type_parameters");

    NonTerminal variant_type_parameter = new NonTerminal("variant_type_parameter");

    NonTerminal variant_type_parameter_list = new NonTerminal("variant_type_parameter_list");
    NonTerminal variant_type_parameter_list_opt = new NonTerminal("variant_type_parameter_list_opt");

    NonTerminal parameter_modifier_opt = new NonTerminal("parameter_modifier_opt");
    NonTerminal parameter_modifier = new NonTerminal("parameter_modifier");
    NonTerminal accessor_modifier_opt = new NonTerminal("accessor_modifier_opt");

    NonTerminal variable_initializer = new NonTerminal("variable_initializer");
    NonTerminal property_declaration_value = new NonTerminal("property_declaration_value");
    NonTerminal property_declaration_value_opt = new NonTerminal("property_declaration_value_opt");
    NonTerminal property_declaration_read_value = new NonTerminal("property_declaration_read_value");
    NonTerminal variable_declarator = new NonTerminal("variable_declarator");
    NonTerminal variable_initializers = new NonTerminal("variable_initializers");
    NonTerminal variable_initializers_opt = new NonTerminal("variable_initializers_opt");
    NonTerminal variable_initializers_opt_opt = new NonTerminal("variable_initializers_opt_opt");
    NonTerminal parameter_array = new NonTerminal("parameter_array");
    NonTerminal p_variable_initializer = new NonTerminal("p_variable_initializer");
    NonTerminal p_variable_initializer_opt = new NonTerminal("p_variable_initializer_opt");
    NonTerminal enum_base = new NonTerminal("enum_base");
    NonTerminal enum_member_declaration = new NonTerminal("enum_member_declaration");
    NonTerminal enum_member_declarations = new NonTerminal("enum_member_declarations");
    NonTerminal enum_member_declarations_opt = new NonTerminal("enum_member_declarations_opt");
    NonTerminal enum_member_declarations_opt_opt = new NonTerminal("enum_member_declarations_opt_opt");
    NonTerminal expression_get = new NonTerminal("expression_get");
    NonTerminal expression_get_opt = new NonTerminal("expression_get_opt");
    NonTerminal fixed_pointer_declarator = new NonTerminal("fixed_pointer_declarator");
    NonTerminal fixed_pointer_initializer = new NonTerminal("fixed_pointer_initializer");
    NonTerminal local_variable_initializer_unsafe = new NonTerminal("local_variable_initializer_unsafe");
    NonTerminal ket_expression = new NonTerminal("ket_expression");
    NonTerminal member_declarator_list_opt_opt = new NonTerminal("member_declarator_list_opt_opt");
    NonTerminal member_declarator_list_opt = new NonTerminal("member_declarator_list_opt");
    NonTerminal statement_list_opt = new NonTerminal("statement_list_opt");
    NonTerminal local_variable_declarators = new NonTerminal("local_variable_declarators");
    NonTerminal local_variable_declarator = new NonTerminal("local_variable_declarator");
    NonTerminal local_variable_initializer = new NonTerminal("local_variable_initializer");
    NonTerminal local_variable_initializer_get_opt = new NonTerminal("local_variable_initializer_get_opt");
    NonTerminal local_variable_initializer_get = new NonTerminal("local_variable_initializer_get");
    NonTerminal switch_labels = new NonTerminal("switch_labels");
    NonTerminal expressions = new NonTerminal("expressions");
    NonTerminal identifier_opt = new NonTerminal("identifier_opt");
    NonTerminal event_accessor_declarations_opt = new NonTerminal("event_accessor_declarations_opt");
    NonTerminal event_declaration_first = new NonTerminal("event_declaration_first");
    NonTerminal constructor_initializer_opt = new NonTerminal("constructor_initializer_opt");
    NonTerminal constructor_initializer = new NonTerminal("constructor_initializer_opt");
    NonTerminal parameter_array_comma = new NonTerminal("parameter_array_comma");
    NonTerminal parameter_array_opt = new NonTerminal("parameter_array_opt");
    NonTerminal fixed_parameters = new NonTerminal("fixed_parameters");
    NonTerminal fixed_parameter = new NonTerminal("fixed_parameter");

    private void qmarkConflict(ParsingContext context, CustomParserAction customAction)
    {
      var scanner = context.Parser.Scanner;
      string previewSym = null;
      if (context.CurrentParserInput.Term.Name == "?")
      {
        scanner.BeginPreview();
        int ltCount = 0;
        while (true)
        {
          //Find first token ahead (using preview mode) that is either end of generic parameter (">") or something else
          Token preview;
          do
          {
            preview = scanner.GetToken();
          } while (_skipTokensInPreview2.Contains(preview.Terminal) && preview.Terminal != base.Eof);
          //See what did we find
          previewSym = preview.Terminal.Name;
          if (previewSym == "?")
            ltCount++;
          
          else
            break;
        }
        scanner.EndPreview(true); //keep previewed tokens; important to keep ">>" matched to two ">" symbols, not one combined symbol (see method below)
      }//if
       //if we see ">", then it is type argument, not operator
      ParserAction action;
      
        action = customAction.ReduceActions.First();
      // Actually execute action
      action.Execute(context);
    }
    private void ResolveLessThanConflict(ParsingContext context, CustomParserAction customAction)
    {
      var scanner = context.Parser.Scanner;
      string previewSym = null;
      if (context.CurrentParserInput.Term.Name == "<")
      {
        scanner.BeginPreview();
        int ltCount = 0;
        while (true)
        {
          //Find first token ahead (using preview mode) that is either end of generic parameter (">") or something else
          Token preview;
          do
          {
            preview = scanner.GetToken();
          } while (_skipTokensInPreview.Contains(preview.Terminal) && preview.Terminal != base.Eof);
          //See what did we find
          previewSym = preview.Terminal.Name;
          if (previewSym == "<")
            ltCount++;
          else if (previewSym == ">" && ltCount > 0)
          {
            ltCount--;
            continue;
          }
          else
            break;
        }
        scanner.EndPreview(true); //keep previewed tokens; important to keep ">>" matched to two ">" symbols, not one combined symbol (see method below)
      }//if
       //if we see ">", then it is type argument, not operator
      ParserAction action;
      if (previewSym == ">")
        action = customAction.ShiftActions.First(a => a.Term.Name == "<");
      else
        action = customAction.ReduceActions.First();
      // Actually execute action
      action.Execute(context);
    }
  }//class
}//namespace