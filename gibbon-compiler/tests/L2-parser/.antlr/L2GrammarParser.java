// Generated from /home/anderslt/gibbon-compiler/gibbon-compiler/tests/L2-parser/L2Grammar.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class L2GrammarParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, T__25=26, T__26=27, T__27=28, T__28=29, T__29=30, T__30=31, 
		T__31=32, T__32=33, T__33=34, T__34=35, T__35=36, T__36=37, T__37=38, 
		T__38=39, T__39=40, T__40=41, T__41=42, T__42=43, T__43=44, T__44=45, 
		T__45=46, T__46=47, T__47=48, VAR=49, INT=50, FLOAT=51, BOOL=52, STRING=53, 
		WS=54;
	public static final int
		RULE_program = 0, RULE_datatypeDecl = 1, RULE_funcDecl = 2, RULE_locatedType = 3, 
		RULE_typeScheme = 4, RULE_val = 5, RULE_letExpress = 6, RULE_letLocExpress = 7, 
		RULE_letRegionExpress = 8, RULE_caseExpress = 9, RULE_funcAppExpress = 10, 
		RULE_dataConAppExpress = 11, RULE_expr = 12, RULE_logicalOrExpr = 13, 
		RULE_logicalAndExpr = 14, RULE_equalityExpr = 15, RULE_relationalExpr = 16, 
		RULE_addExpr = 17, RULE_mulExpr = 18, RULE_powExpr = 19, RULE_atom = 20, 
		RULE_baseType = 21, RULE_lit = 22, RULE_binaryOp = 23, RULE_pat = 24, 
		RULE_locExpress = 25, RULE_locRegion = 26, RULE_concreteLoc = 27, RULE_funcVar = 28, 
		RULE_regionVar = 29, RULE_locVar = 30, RULE_indexVar = 31, RULE_typeCon = 32, 
		RULE_dataCon = 33;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "datatypeDecl", "funcDecl", "locatedType", "typeScheme", "val", 
			"letExpress", "letLocExpress", "letRegionExpress", "caseExpress", "funcAppExpress", 
			"dataConAppExpress", "expr", "logicalOrExpr", "logicalAndExpr", "equalityExpr", 
			"relationalExpr", "addExpr", "mulExpr", "powExpr", "atom", "baseType", 
			"lit", "binaryOp", "pat", "locExpress", "locRegion", "concreteLoc", "funcVar", 
			"regionVar", "locVar", "indexVar", "typeCon", "dataCon"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'data'", "'='", "'|'", "':'", "'['", "']'", "'@'", "'->'", "'let'", 
			"'in'", "'letloc'", "'letregion'", "'case'", "'of'", "'||'", "'&&'", 
			"'=='", "'.==.'", "'*==*'", "'/='", "'>'", "'<'", "'.>.'", "'.<.'", "'>='", 
			"'<='", "'.<=.'", "'.>=.'", "'+'", "'-'", "'.+.'", "'.-.'", "'*'", "'/'", 
			"'`div`'", "'`mod`'", "'.*.'", "'./.'", "'^'", "'('", "')'", "'Int'", 
			"'Float'", "'Bool'", "'String'", "'start'", "'after'", "','"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, "VAR", "INT", "FLOAT", "BOOL", "STRING", "WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "L2Grammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public L2GrammarParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode EOF() { return getToken(L2GrammarParser.EOF, 0); }
		public List<DatatypeDeclContext> datatypeDecl() {
			return getRuleContexts(DatatypeDeclContext.class);
		}
		public DatatypeDeclContext datatypeDecl(int i) {
			return getRuleContext(DatatypeDeclContext.class,i);
		}
		public List<FuncDeclContext> funcDecl() {
			return getRuleContexts(FuncDeclContext.class);
		}
		public FuncDeclContext funcDecl(int i) {
			return getRuleContext(FuncDeclContext.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(71);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(68);
				datatypeDecl();
				}
				}
				setState(73);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(77);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(74);
					funcDecl();
					}
					} 
				}
				setState(79);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(80);
			expr();
			setState(81);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DatatypeDeclContext extends ParserRuleContext {
		public List<TypeConContext> typeCon() {
			return getRuleContexts(TypeConContext.class);
		}
		public TypeConContext typeCon(int i) {
			return getRuleContext(TypeConContext.class,i);
		}
		public List<DataConContext> dataCon() {
			return getRuleContexts(DataConContext.class);
		}
		public DataConContext dataCon(int i) {
			return getRuleContext(DataConContext.class,i);
		}
		public DatatypeDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_datatypeDecl; }
	}

	public final DatatypeDeclContext datatypeDecl() throws RecognitionException {
		DatatypeDeclContext _localctx = new DatatypeDeclContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_datatypeDecl);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(83);
			match(T__0);
			setState(84);
			typeCon();
			setState(85);
			match(T__1);
			{
			setState(86);
			dataCon();
			setState(90);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(87);
					typeCon();
					}
					} 
				}
				setState(92);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			}
			setState(103);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2) {
				{
				{
				setState(93);
				match(T__2);
				{
				setState(94);
				dataCon();
				setState(98);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(95);
						typeCon();
						}
						} 
					}
					setState(100);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
				}
				}
				}
				}
				setState(105);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FuncDeclContext extends ParserRuleContext {
		public List<FuncVarContext> funcVar() {
			return getRuleContexts(FuncVarContext.class);
		}
		public FuncVarContext funcVar(int i) {
			return getRuleContext(FuncVarContext.class,i);
		}
		public TypeSchemeContext typeScheme() {
			return getRuleContext(TypeSchemeContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<LocRegionContext> locRegion() {
			return getRuleContexts(LocRegionContext.class);
		}
		public LocRegionContext locRegion(int i) {
			return getRuleContext(LocRegionContext.class,i);
		}
		public List<TerminalNode> VAR() { return getTokens(L2GrammarParser.VAR); }
		public TerminalNode VAR(int i) {
			return getToken(L2GrammarParser.VAR, i);
		}
		public FuncDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcDecl; }
	}

	public final FuncDeclContext funcDecl() throws RecognitionException {
		FuncDeclContext _localctx = new FuncDeclContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_funcDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(106);
			funcVar();
			setState(107);
			match(T__3);
			setState(108);
			typeScheme();
			setState(109);
			funcVar();
			setState(110);
			match(T__4);
			setState(114);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__39) {
				{
				{
				setState(111);
				locRegion();
				}
				}
				setState(116);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(117);
			match(T__5);
			setState(121);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==VAR) {
				{
				{
				setState(118);
				match(VAR);
				}
				}
				setState(123);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(124);
			match(T__1);
			setState(125);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LocatedTypeContext extends ParserRuleContext {
		public TypeConContext typeCon() {
			return getRuleContext(TypeConContext.class,0);
		}
		public LocRegionContext locRegion() {
			return getRuleContext(LocRegionContext.class,0);
		}
		public LocatedTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_locatedType; }
	}

	public final LocatedTypeContext locatedType() throws RecognitionException {
		LocatedTypeContext _localctx = new LocatedTypeContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_locatedType);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(127);
			typeCon();
			setState(128);
			match(T__6);
			setState(129);
			locRegion();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeSchemeContext extends ParserRuleContext {
		public List<LocatedTypeContext> locatedType() {
			return getRuleContexts(LocatedTypeContext.class);
		}
		public LocatedTypeContext locatedType(int i) {
			return getRuleContext(LocatedTypeContext.class,i);
		}
		public List<BaseTypeContext> baseType() {
			return getRuleContexts(BaseTypeContext.class);
		}
		public BaseTypeContext baseType(int i) {
			return getRuleContext(BaseTypeContext.class,i);
		}
		public TypeSchemeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeScheme; }
	}

	public final TypeSchemeContext typeScheme() throws RecognitionException {
		TypeSchemeContext _localctx = new TypeSchemeContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_typeScheme);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(139);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(133);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case VAR:
						{
						setState(131);
						locatedType();
						}
						break;
					case T__41:
					case T__42:
					case T__43:
					case T__44:
						{
						setState(132);
						baseType();
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(135);
					match(T__7);
					}
					} 
				}
				setState(141);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			}
			setState(144);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case VAR:
				{
				setState(142);
				locatedType();
				}
				break;
			case T__41:
			case T__42:
			case T__43:
			case T__44:
				{
				setState(143);
				baseType();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public ConcreteLocContext concreteLoc() {
			return getRuleContext(ConcreteLocContext.class,0);
		}
		public LitContext lit() {
			return getRuleContext(LitContext.class,0);
		}
		public ValContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_val; }
	}

	public final ValContext val() throws RecognitionException {
		ValContext _localctx = new ValContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_val);
		try {
			setState(149);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case VAR:
				enterOuterAlt(_localctx, 1);
				{
				setState(146);
				match(VAR);
				}
				break;
			case T__39:
				enterOuterAlt(_localctx, 2);
				{
				setState(147);
				concreteLoc();
				}
				break;
			case INT:
			case FLOAT:
			case BOOL:
			case STRING:
				enterOuterAlt(_localctx, 3);
				{
				setState(148);
				lit();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetExpressContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public LocatedTypeContext locatedType() {
			return getRuleContext(LocatedTypeContext.class,0);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public LetExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_letExpress; }
	}

	public final LetExpressContext letExpress() throws RecognitionException {
		LetExpressContext _localctx = new LetExpressContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_letExpress);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(151);
			match(T__8);
			setState(152);
			match(VAR);
			setState(153);
			match(T__3);
			setState(154);
			locatedType();
			setState(155);
			match(T__1);
			setState(156);
			expr();
			setState(157);
			match(T__9);
			setState(158);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetLocExpressContext extends ParserRuleContext {
		public LocRegionContext locRegion() {
			return getRuleContext(LocRegionContext.class,0);
		}
		public LocExpressContext locExpress() {
			return getRuleContext(LocExpressContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public LetLocExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_letLocExpress; }
	}

	public final LetLocExpressContext letLocExpress() throws RecognitionException {
		LetLocExpressContext _localctx = new LetLocExpressContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_letLocExpress);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(160);
			match(T__10);
			setState(161);
			locRegion();
			setState(162);
			match(T__1);
			setState(163);
			locExpress();
			setState(164);
			match(T__9);
			setState(165);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetRegionExpressContext extends ParserRuleContext {
		public RegionVarContext regionVar() {
			return getRuleContext(RegionVarContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public LetRegionExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_letRegionExpress; }
	}

	public final LetRegionExpressContext letRegionExpress() throws RecognitionException {
		LetRegionExpressContext _localctx = new LetRegionExpressContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_letRegionExpress);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(167);
			match(T__11);
			setState(168);
			regionVar();
			setState(169);
			match(T__9);
			setState(170);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CaseExpressContext extends ParserRuleContext {
		public ValContext val() {
			return getRuleContext(ValContext.class,0);
		}
		public List<PatContext> pat() {
			return getRuleContexts(PatContext.class);
		}
		public PatContext pat(int i) {
			return getRuleContext(PatContext.class,i);
		}
		public CaseExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_caseExpress; }
	}

	public final CaseExpressContext caseExpress() throws RecognitionException {
		CaseExpressContext _localctx = new CaseExpressContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_caseExpress);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(172);
			match(T__12);
			setState(173);
			val();
			setState(174);
			match(T__13);
			setState(176); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(175);
					pat();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(178); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FuncAppExpressContext extends ParserRuleContext {
		public FuncVarContext funcVar() {
			return getRuleContext(FuncVarContext.class,0);
		}
		public List<LocRegionContext> locRegion() {
			return getRuleContexts(LocRegionContext.class);
		}
		public LocRegionContext locRegion(int i) {
			return getRuleContext(LocRegionContext.class,i);
		}
		public List<ValContext> val() {
			return getRuleContexts(ValContext.class);
		}
		public ValContext val(int i) {
			return getRuleContext(ValContext.class,i);
		}
		public FuncAppExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcAppExpress; }
	}

	public final FuncAppExpressContext funcAppExpress() throws RecognitionException {
		FuncAppExpressContext _localctx = new FuncAppExpressContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_funcAppExpress);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(180);
			funcVar();
			setState(181);
			match(T__4);
			setState(185);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__39) {
				{
				{
				setState(182);
				locRegion();
				}
				}
				setState(187);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(188);
			match(T__5);
			setState(192);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,13,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(189);
					val();
					}
					} 
				}
				setState(194);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,13,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DataConAppExpressContext extends ParserRuleContext {
		public DataConContext dataCon() {
			return getRuleContext(DataConContext.class,0);
		}
		public LocRegionContext locRegion() {
			return getRuleContext(LocRegionContext.class,0);
		}
		public List<ValContext> val() {
			return getRuleContexts(ValContext.class);
		}
		public ValContext val(int i) {
			return getRuleContext(ValContext.class,i);
		}
		public DataConAppExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dataConAppExpress; }
	}

	public final DataConAppExpressContext dataConAppExpress() throws RecognitionException {
		DataConAppExpressContext _localctx = new DataConAppExpressContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_dataConAppExpress);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(195);
			dataCon();
			setState(196);
			locRegion();
			setState(200);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(197);
					val();
					}
					} 
				}
				setState(202);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public LogicalOrExprContext logicalOrExpr() {
			return getRuleContext(LogicalOrExprContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(203);
			logicalOrExpr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LogicalOrExprContext extends ParserRuleContext {
		public List<LogicalAndExprContext> logicalAndExpr() {
			return getRuleContexts(LogicalAndExprContext.class);
		}
		public LogicalAndExprContext logicalAndExpr(int i) {
			return getRuleContext(LogicalAndExprContext.class,i);
		}
		public LogicalOrExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logicalOrExpr; }
	}

	public final LogicalOrExprContext logicalOrExpr() throws RecognitionException {
		LogicalOrExprContext _localctx = new LogicalOrExprContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_logicalOrExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(205);
			logicalAndExpr();
			setState(210);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(206);
					match(T__14);
					setState(207);
					logicalAndExpr();
					}
					} 
				}
				setState(212);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LogicalAndExprContext extends ParserRuleContext {
		public List<EqualityExprContext> equalityExpr() {
			return getRuleContexts(EqualityExprContext.class);
		}
		public EqualityExprContext equalityExpr(int i) {
			return getRuleContext(EqualityExprContext.class,i);
		}
		public LogicalAndExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logicalAndExpr; }
	}

	public final LogicalAndExprContext logicalAndExpr() throws RecognitionException {
		LogicalAndExprContext _localctx = new LogicalAndExprContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_logicalAndExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(213);
			equalityExpr();
			setState(218);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,16,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(214);
					match(T__15);
					setState(215);
					equalityExpr();
					}
					} 
				}
				setState(220);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,16,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class EqualityExprContext extends ParserRuleContext {
		public List<RelationalExprContext> relationalExpr() {
			return getRuleContexts(RelationalExprContext.class);
		}
		public RelationalExprContext relationalExpr(int i) {
			return getRuleContext(RelationalExprContext.class,i);
		}
		public EqualityExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_equalityExpr; }
	}

	public final EqualityExprContext equalityExpr() throws RecognitionException {
		EqualityExprContext _localctx = new EqualityExprContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_equalityExpr);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(221);
			relationalExpr();
			setState(226);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,17,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(222);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 1966080L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(223);
					relationalExpr();
					}
					} 
				}
				setState(228);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,17,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RelationalExprContext extends ParserRuleContext {
		public List<AddExprContext> addExpr() {
			return getRuleContexts(AddExprContext.class);
		}
		public AddExprContext addExpr(int i) {
			return getRuleContext(AddExprContext.class,i);
		}
		public RelationalExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_relationalExpr; }
	}

	public final RelationalExprContext relationalExpr() throws RecognitionException {
		RelationalExprContext _localctx = new RelationalExprContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_relationalExpr);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(229);
			addExpr();
			setState(234);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(230);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 534773760L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(231);
					addExpr();
					}
					} 
				}
				setState(236);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AddExprContext extends ParserRuleContext {
		public List<MulExprContext> mulExpr() {
			return getRuleContexts(MulExprContext.class);
		}
		public MulExprContext mulExpr(int i) {
			return getRuleContext(MulExprContext.class,i);
		}
		public AddExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_addExpr; }
	}

	public final AddExprContext addExpr() throws RecognitionException {
		AddExprContext _localctx = new AddExprContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_addExpr);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(237);
			mulExpr();
			setState(242);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,19,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(238);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 8053063680L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(239);
					mulExpr();
					}
					} 
				}
				setState(244);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,19,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class MulExprContext extends ParserRuleContext {
		public List<PowExprContext> powExpr() {
			return getRuleContexts(PowExprContext.class);
		}
		public PowExprContext powExpr(int i) {
			return getRuleContext(PowExprContext.class,i);
		}
		public MulExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_mulExpr; }
	}

	public final MulExprContext mulExpr() throws RecognitionException {
		MulExprContext _localctx = new MulExprContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_mulExpr);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(245);
			powExpr();
			setState(250);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,20,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(246);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 541165879296L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(247);
					powExpr();
					}
					} 
				}
				setState(252);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,20,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PowExprContext extends ParserRuleContext {
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public List<PowExprContext> powExpr() {
			return getRuleContexts(PowExprContext.class);
		}
		public PowExprContext powExpr(int i) {
			return getRuleContext(PowExprContext.class,i);
		}
		public PowExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_powExpr; }
	}

	public final PowExprContext powExpr() throws RecognitionException {
		PowExprContext _localctx = new PowExprContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_powExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(253);
			atom();
			setState(258);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					{
					setState(254);
					match(T__38);
					}
					setState(255);
					powExpr();
					}
					} 
				}
				setState(260);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AtomContext extends ParserRuleContext {
		public ValContext val() {
			return getRuleContext(ValContext.class,0);
		}
		public FuncAppExpressContext funcAppExpress() {
			return getRuleContext(FuncAppExpressContext.class,0);
		}
		public DataConAppExpressContext dataConAppExpress() {
			return getRuleContext(DataConAppExpressContext.class,0);
		}
		public LetExpressContext letExpress() {
			return getRuleContext(LetExpressContext.class,0);
		}
		public LetLocExpressContext letLocExpress() {
			return getRuleContext(LetLocExpressContext.class,0);
		}
		public LetRegionExpressContext letRegionExpress() {
			return getRuleContext(LetRegionExpressContext.class,0);
		}
		public CaseExpressContext caseExpress() {
			return getRuleContext(CaseExpressContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public AtomContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_atom; }
	}

	public final AtomContext atom() throws RecognitionException {
		AtomContext _localctx = new AtomContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_atom);
		try {
			setState(272);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,22,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(261);
				val();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(262);
				funcAppExpress();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(263);
				dataConAppExpress();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(264);
				letExpress();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(265);
				letLocExpress();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(266);
				letRegionExpress();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(267);
				caseExpress();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(268);
				match(T__39);
				setState(269);
				expr();
				setState(270);
				match(T__40);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BaseTypeContext extends ParserRuleContext {
		public BaseTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_baseType; }
	}

	public final BaseTypeContext baseType() throws RecognitionException {
		BaseTypeContext _localctx = new BaseTypeContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_baseType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(274);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 65970697666560L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LitContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(L2GrammarParser.INT, 0); }
		public TerminalNode FLOAT() { return getToken(L2GrammarParser.FLOAT, 0); }
		public TerminalNode BOOL() { return getToken(L2GrammarParser.BOOL, 0); }
		public TerminalNode STRING() { return getToken(L2GrammarParser.STRING, 0); }
		public LitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lit; }
	}

	public final LitContext lit() throws RecognitionException {
		LitContext _localctx = new LitContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_lit);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(276);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 16888498602639360L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BinaryOpContext extends ParserRuleContext {
		public BinaryOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_binaryOp; }
	}

	public final BinaryOpContext binaryOp() throws RecognitionException {
		BinaryOpContext _localctx = new BinaryOpContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_binaryOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(278);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 1099511595008L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PatContext extends ParserRuleContext {
		public DataConContext dataCon() {
			return getRuleContext(DataConContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<ValContext> val() {
			return getRuleContexts(ValContext.class);
		}
		public ValContext val(int i) {
			return getRuleContext(ValContext.class,i);
		}
		public List<LocatedTypeContext> locatedType() {
			return getRuleContexts(LocatedTypeContext.class);
		}
		public LocatedTypeContext locatedType(int i) {
			return getRuleContext(LocatedTypeContext.class,i);
		}
		public PatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pat; }
	}

	public final PatContext pat() throws RecognitionException {
		PatContext _localctx = new PatContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_pat);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(280);
			dataCon();
			setState(281);
			match(T__39);
			setState(288);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 17452548067688448L) != 0)) {
				{
				{
				setState(282);
				val();
				setState(283);
				match(T__3);
				setState(284);
				locatedType();
				}
				}
				setState(290);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(291);
			match(T__40);
			setState(292);
			match(T__7);
			setState(293);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LocExpressContext extends ParserRuleContext {
		public RegionVarContext regionVar() {
			return getRuleContext(RegionVarContext.class,0);
		}
		public LocRegionContext locRegion() {
			return getRuleContext(LocRegionContext.class,0);
		}
		public TerminalNode INT() { return getToken(L2GrammarParser.INT, 0); }
		public LocatedTypeContext locatedType() {
			return getRuleContext(LocatedTypeContext.class,0);
		}
		public LocExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_locExpress; }
	}

	public final LocExpressContext locExpress() throws RecognitionException {
		LocExpressContext _localctx = new LocExpressContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_locExpress);
		try {
			setState(311);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,24,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(295);
				match(T__39);
				setState(296);
				match(T__45);
				setState(297);
				regionVar();
				setState(298);
				match(T__40);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(300);
				match(T__39);
				setState(301);
				locRegion();
				setState(302);
				match(T__28);
				setState(303);
				match(INT);
				setState(304);
				match(T__40);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(306);
				match(T__39);
				setState(307);
				match(T__46);
				setState(308);
				locatedType();
				setState(309);
				match(T__40);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LocRegionContext extends ParserRuleContext {
		public LocVarContext locVar() {
			return getRuleContext(LocVarContext.class,0);
		}
		public RegionVarContext regionVar() {
			return getRuleContext(RegionVarContext.class,0);
		}
		public IndexVarContext indexVar() {
			return getRuleContext(IndexVarContext.class,0);
		}
		public LocRegionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_locRegion; }
	}

	public final LocRegionContext locRegion() throws RecognitionException {
		LocRegionContext _localctx = new LocRegionContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_locRegion);
		try {
			setState(327);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,25,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(313);
				match(T__39);
				setState(314);
				locVar();
				setState(315);
				match(T__47);
				setState(316);
				regionVar();
				setState(317);
				match(T__40);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(319);
				match(T__39);
				setState(320);
				locVar();
				setState(321);
				match(T__47);
				setState(322);
				regionVar();
				setState(323);
				match(T__47);
				setState(324);
				indexVar();
				setState(325);
				match(T__40);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ConcreteLocContext extends ParserRuleContext {
		public RegionVarContext regionVar() {
			return getRuleContext(RegionVarContext.class,0);
		}
		public IndexVarContext indexVar() {
			return getRuleContext(IndexVarContext.class,0);
		}
		public LocRegionContext locRegion() {
			return getRuleContext(LocRegionContext.class,0);
		}
		public ConcreteLocContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_concreteLoc; }
	}

	public final ConcreteLocContext concreteLoc() throws RecognitionException {
		ConcreteLocContext _localctx = new ConcreteLocContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_concreteLoc);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(329);
			match(T__39);
			setState(330);
			regionVar();
			setState(331);
			match(T__47);
			setState(332);
			indexVar();
			setState(333);
			match(T__47);
			setState(334);
			locRegion();
			setState(335);
			match(T__40);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FuncVarContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public FuncVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcVar; }
	}

	public final FuncVarContext funcVar() throws RecognitionException {
		FuncVarContext _localctx = new FuncVarContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_funcVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(337);
			match(VAR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RegionVarContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public RegionVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_regionVar; }
	}

	public final RegionVarContext regionVar() throws RecognitionException {
		RegionVarContext _localctx = new RegionVarContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_regionVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(339);
			match(VAR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LocVarContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public LocVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_locVar; }
	}

	public final LocVarContext locVar() throws RecognitionException {
		LocVarContext _localctx = new LocVarContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_locVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(341);
			match(VAR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class IndexVarContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public IndexVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_indexVar; }
	}

	public final IndexVarContext indexVar() throws RecognitionException {
		IndexVarContext _localctx = new IndexVarContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_indexVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(343);
			match(VAR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeConContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public TypeConContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeCon; }
	}

	public final TypeConContext typeCon() throws RecognitionException {
		TypeConContext _localctx = new TypeConContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_typeCon);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(345);
			match(VAR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DataConContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public DataConContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dataCon; }
	}

	public final DataConContext dataCon() throws RecognitionException {
		DataConContext _localctx = new DataConContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_dataCon);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(347);
			match(VAR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u00016\u015e\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012"+
		"\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015"+
		"\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007\u0018"+
		"\u0002\u0019\u0007\u0019\u0002\u001a\u0007\u001a\u0002\u001b\u0007\u001b"+
		"\u0002\u001c\u0007\u001c\u0002\u001d\u0007\u001d\u0002\u001e\u0007\u001e"+
		"\u0002\u001f\u0007\u001f\u0002 \u0007 \u0002!\u0007!\u0001\u0000\u0005"+
		"\u0000F\b\u0000\n\u0000\f\u0000I\t\u0000\u0001\u0000\u0005\u0000L\b\u0000"+
		"\n\u0000\f\u0000O\t\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0005\u0001Y\b\u0001"+
		"\n\u0001\f\u0001\\\t\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0005\u0001"+
		"a\b\u0001\n\u0001\f\u0001d\t\u0001\u0005\u0001f\b\u0001\n\u0001\f\u0001"+
		"i\t\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0005\u0002q\b\u0002\n\u0002\f\u0002t\t\u0002\u0001\u0002"+
		"\u0001\u0002\u0005\u0002x\b\u0002\n\u0002\f\u0002{\t\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0001\u0004\u0001\u0004\u0003\u0004\u0086\b\u0004\u0001\u0004\u0001\u0004"+
		"\u0005\u0004\u008a\b\u0004\n\u0004\f\u0004\u008d\t\u0004\u0001\u0004\u0001"+
		"\u0004\u0003\u0004\u0091\b\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0003"+
		"\u0005\u0096\b\u0005\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0007\u0001"+
		"\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\t\u0001\t\u0001\t\u0001\t\u0004"+
		"\t\u00b1\b\t\u000b\t\f\t\u00b2\u0001\n\u0001\n\u0001\n\u0005\n\u00b8\b"+
		"\n\n\n\f\n\u00bb\t\n\u0001\n\u0001\n\u0005\n\u00bf\b\n\n\n\f\n\u00c2\t"+
		"\n\u0001\u000b\u0001\u000b\u0001\u000b\u0005\u000b\u00c7\b\u000b\n\u000b"+
		"\f\u000b\u00ca\t\u000b\u0001\f\u0001\f\u0001\r\u0001\r\u0001\r\u0005\r"+
		"\u00d1\b\r\n\r\f\r\u00d4\t\r\u0001\u000e\u0001\u000e\u0001\u000e\u0005"+
		"\u000e\u00d9\b\u000e\n\u000e\f\u000e\u00dc\t\u000e\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0005\u000f\u00e1\b\u000f\n\u000f\f\u000f\u00e4\t\u000f\u0001"+
		"\u0010\u0001\u0010\u0001\u0010\u0005\u0010\u00e9\b\u0010\n\u0010\f\u0010"+
		"\u00ec\t\u0010\u0001\u0011\u0001\u0011\u0001\u0011\u0005\u0011\u00f1\b"+
		"\u0011\n\u0011\f\u0011\u00f4\t\u0011\u0001\u0012\u0001\u0012\u0001\u0012"+
		"\u0005\u0012\u00f9\b\u0012\n\u0012\f\u0012\u00fc\t\u0012\u0001\u0013\u0001"+
		"\u0013\u0001\u0013\u0005\u0013\u0101\b\u0013\n\u0013\f\u0013\u0104\t\u0013"+
		"\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014"+
		"\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0003\u0014"+
		"\u0111\b\u0014\u0001\u0015\u0001\u0015\u0001\u0016\u0001\u0016\u0001\u0017"+
		"\u0001\u0017\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0018\u0005\u0018\u011f\b\u0018\n\u0018\f\u0018\u0122\t\u0018\u0001"+
		"\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0019\u0001\u0019\u0001"+
		"\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001"+
		"\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001"+
		"\u0019\u0001\u0019\u0003\u0019\u0138\b\u0019\u0001\u001a\u0001\u001a\u0001"+
		"\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001"+
		"\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0001\u001a\u0003"+
		"\u001a\u0148\b\u001a\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001b\u0001"+
		"\u001b\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001c\u0001\u001c\u0001"+
		"\u001d\u0001\u001d\u0001\u001e\u0001\u001e\u0001\u001f\u0001\u001f\u0001"+
		" \u0001 \u0001!\u0001!\u0001!\u0000\u0000\"\u0000\u0002\u0004\u0006\b"+
		"\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e \"$&(*,.02"+
		"468:<>@B\u0000\u0007\u0001\u0000\u0011\u0014\u0001\u0000\u0015\u001c\u0001"+
		"\u0000\u001d \u0001\u0000!&\u0001\u0000*-\u0001\u000025\u0001\u0000\u000f"+
		"\'\u015d\u0000G\u0001\u0000\u0000\u0000\u0002S\u0001\u0000\u0000\u0000"+
		"\u0004j\u0001\u0000\u0000\u0000\u0006\u007f\u0001\u0000\u0000\u0000\b"+
		"\u008b\u0001\u0000\u0000\u0000\n\u0095\u0001\u0000\u0000\u0000\f\u0097"+
		"\u0001\u0000\u0000\u0000\u000e\u00a0\u0001\u0000\u0000\u0000\u0010\u00a7"+
		"\u0001\u0000\u0000\u0000\u0012\u00ac\u0001\u0000\u0000\u0000\u0014\u00b4"+
		"\u0001\u0000\u0000\u0000\u0016\u00c3\u0001\u0000\u0000\u0000\u0018\u00cb"+
		"\u0001\u0000\u0000\u0000\u001a\u00cd\u0001\u0000\u0000\u0000\u001c\u00d5"+
		"\u0001\u0000\u0000\u0000\u001e\u00dd\u0001\u0000\u0000\u0000 \u00e5\u0001"+
		"\u0000\u0000\u0000\"\u00ed\u0001\u0000\u0000\u0000$\u00f5\u0001\u0000"+
		"\u0000\u0000&\u00fd\u0001\u0000\u0000\u0000(\u0110\u0001\u0000\u0000\u0000"+
		"*\u0112\u0001\u0000\u0000\u0000,\u0114\u0001\u0000\u0000\u0000.\u0116"+
		"\u0001\u0000\u0000\u00000\u0118\u0001\u0000\u0000\u00002\u0137\u0001\u0000"+
		"\u0000\u00004\u0147\u0001\u0000\u0000\u00006\u0149\u0001\u0000\u0000\u0000"+
		"8\u0151\u0001\u0000\u0000\u0000:\u0153\u0001\u0000\u0000\u0000<\u0155"+
		"\u0001\u0000\u0000\u0000>\u0157\u0001\u0000\u0000\u0000@\u0159\u0001\u0000"+
		"\u0000\u0000B\u015b\u0001\u0000\u0000\u0000DF\u0003\u0002\u0001\u0000"+
		"ED\u0001\u0000\u0000\u0000FI\u0001\u0000\u0000\u0000GE\u0001\u0000\u0000"+
		"\u0000GH\u0001\u0000\u0000\u0000HM\u0001\u0000\u0000\u0000IG\u0001\u0000"+
		"\u0000\u0000JL\u0003\u0004\u0002\u0000KJ\u0001\u0000\u0000\u0000LO\u0001"+
		"\u0000\u0000\u0000MK\u0001\u0000\u0000\u0000MN\u0001\u0000\u0000\u0000"+
		"NP\u0001\u0000\u0000\u0000OM\u0001\u0000\u0000\u0000PQ\u0003\u0018\f\u0000"+
		"QR\u0005\u0000\u0000\u0001R\u0001\u0001\u0000\u0000\u0000ST\u0005\u0001"+
		"\u0000\u0000TU\u0003@ \u0000UV\u0005\u0002\u0000\u0000VZ\u0003B!\u0000"+
		"WY\u0003@ \u0000XW\u0001\u0000\u0000\u0000Y\\\u0001\u0000\u0000\u0000"+
		"ZX\u0001\u0000\u0000\u0000Z[\u0001\u0000\u0000\u0000[g\u0001\u0000\u0000"+
		"\u0000\\Z\u0001\u0000\u0000\u0000]^\u0005\u0003\u0000\u0000^b\u0003B!"+
		"\u0000_a\u0003@ \u0000`_\u0001\u0000\u0000\u0000ad\u0001\u0000\u0000\u0000"+
		"b`\u0001\u0000\u0000\u0000bc\u0001\u0000\u0000\u0000cf\u0001\u0000\u0000"+
		"\u0000db\u0001\u0000\u0000\u0000e]\u0001\u0000\u0000\u0000fi\u0001\u0000"+
		"\u0000\u0000ge\u0001\u0000\u0000\u0000gh\u0001\u0000\u0000\u0000h\u0003"+
		"\u0001\u0000\u0000\u0000ig\u0001\u0000\u0000\u0000jk\u00038\u001c\u0000"+
		"kl\u0005\u0004\u0000\u0000lm\u0003\b\u0004\u0000mn\u00038\u001c\u0000"+
		"nr\u0005\u0005\u0000\u0000oq\u00034\u001a\u0000po\u0001\u0000\u0000\u0000"+
		"qt\u0001\u0000\u0000\u0000rp\u0001\u0000\u0000\u0000rs\u0001\u0000\u0000"+
		"\u0000su\u0001\u0000\u0000\u0000tr\u0001\u0000\u0000\u0000uy\u0005\u0006"+
		"\u0000\u0000vx\u00051\u0000\u0000wv\u0001\u0000\u0000\u0000x{\u0001\u0000"+
		"\u0000\u0000yw\u0001\u0000\u0000\u0000yz\u0001\u0000\u0000\u0000z|\u0001"+
		"\u0000\u0000\u0000{y\u0001\u0000\u0000\u0000|}\u0005\u0002\u0000\u0000"+
		"}~\u0003\u0018\f\u0000~\u0005\u0001\u0000\u0000\u0000\u007f\u0080\u0003"+
		"@ \u0000\u0080\u0081\u0005\u0007\u0000\u0000\u0081\u0082\u00034\u001a"+
		"\u0000\u0082\u0007\u0001\u0000\u0000\u0000\u0083\u0086\u0003\u0006\u0003"+
		"\u0000\u0084\u0086\u0003*\u0015\u0000\u0085\u0083\u0001\u0000\u0000\u0000"+
		"\u0085\u0084\u0001\u0000\u0000\u0000\u0086\u0087\u0001\u0000\u0000\u0000"+
		"\u0087\u0088\u0005\b\u0000\u0000\u0088\u008a\u0001\u0000\u0000\u0000\u0089"+
		"\u0085\u0001\u0000\u0000\u0000\u008a\u008d\u0001\u0000\u0000\u0000\u008b"+
		"\u0089\u0001\u0000\u0000\u0000\u008b\u008c\u0001\u0000\u0000\u0000\u008c"+
		"\u0090\u0001\u0000\u0000\u0000\u008d\u008b\u0001\u0000\u0000\u0000\u008e"+
		"\u0091\u0003\u0006\u0003\u0000\u008f\u0091\u0003*\u0015\u0000\u0090\u008e"+
		"\u0001\u0000\u0000\u0000\u0090\u008f\u0001\u0000\u0000\u0000\u0091\t\u0001"+
		"\u0000\u0000\u0000\u0092\u0096\u00051\u0000\u0000\u0093\u0096\u00036\u001b"+
		"\u0000\u0094\u0096\u0003,\u0016\u0000\u0095\u0092\u0001\u0000\u0000\u0000"+
		"\u0095\u0093\u0001\u0000\u0000\u0000\u0095\u0094\u0001\u0000\u0000\u0000"+
		"\u0096\u000b\u0001\u0000\u0000\u0000\u0097\u0098\u0005\t\u0000\u0000\u0098"+
		"\u0099\u00051\u0000\u0000\u0099\u009a\u0005\u0004\u0000\u0000\u009a\u009b"+
		"\u0003\u0006\u0003\u0000\u009b\u009c\u0005\u0002\u0000\u0000\u009c\u009d"+
		"\u0003\u0018\f\u0000\u009d\u009e\u0005\n\u0000\u0000\u009e\u009f\u0003"+
		"\u0018\f\u0000\u009f\r\u0001\u0000\u0000\u0000\u00a0\u00a1\u0005\u000b"+
		"\u0000\u0000\u00a1\u00a2\u00034\u001a\u0000\u00a2\u00a3\u0005\u0002\u0000"+
		"\u0000\u00a3\u00a4\u00032\u0019\u0000\u00a4\u00a5\u0005\n\u0000\u0000"+
		"\u00a5\u00a6\u0003\u0018\f\u0000\u00a6\u000f\u0001\u0000\u0000\u0000\u00a7"+
		"\u00a8\u0005\f\u0000\u0000\u00a8\u00a9\u0003:\u001d\u0000\u00a9\u00aa"+
		"\u0005\n\u0000\u0000\u00aa\u00ab\u0003\u0018\f\u0000\u00ab\u0011\u0001"+
		"\u0000\u0000\u0000\u00ac\u00ad\u0005\r\u0000\u0000\u00ad\u00ae\u0003\n"+
		"\u0005\u0000\u00ae\u00b0\u0005\u000e\u0000\u0000\u00af\u00b1\u00030\u0018"+
		"\u0000\u00b0\u00af\u0001\u0000\u0000\u0000\u00b1\u00b2\u0001\u0000\u0000"+
		"\u0000\u00b2\u00b0\u0001\u0000\u0000\u0000\u00b2\u00b3\u0001\u0000\u0000"+
		"\u0000\u00b3\u0013\u0001\u0000\u0000\u0000\u00b4\u00b5\u00038\u001c\u0000"+
		"\u00b5\u00b9\u0005\u0005\u0000\u0000\u00b6\u00b8\u00034\u001a\u0000\u00b7"+
		"\u00b6\u0001\u0000\u0000\u0000\u00b8\u00bb\u0001\u0000\u0000\u0000\u00b9"+
		"\u00b7\u0001\u0000\u0000\u0000\u00b9\u00ba\u0001\u0000\u0000\u0000\u00ba"+
		"\u00bc\u0001\u0000\u0000\u0000\u00bb\u00b9\u0001\u0000\u0000\u0000\u00bc"+
		"\u00c0\u0005\u0006\u0000\u0000\u00bd\u00bf\u0003\n\u0005\u0000\u00be\u00bd"+
		"\u0001\u0000\u0000\u0000\u00bf\u00c2\u0001\u0000\u0000\u0000\u00c0\u00be"+
		"\u0001\u0000\u0000\u0000\u00c0\u00c1\u0001\u0000\u0000\u0000\u00c1\u0015"+
		"\u0001\u0000\u0000\u0000\u00c2\u00c0\u0001\u0000\u0000\u0000\u00c3\u00c4"+
		"\u0003B!\u0000\u00c4\u00c8\u00034\u001a\u0000\u00c5\u00c7\u0003\n\u0005"+
		"\u0000\u00c6\u00c5\u0001\u0000\u0000\u0000\u00c7\u00ca\u0001\u0000\u0000"+
		"\u0000\u00c8\u00c6\u0001\u0000\u0000\u0000\u00c8\u00c9\u0001\u0000\u0000"+
		"\u0000\u00c9\u0017\u0001\u0000\u0000\u0000\u00ca\u00c8\u0001\u0000\u0000"+
		"\u0000\u00cb\u00cc\u0003\u001a\r\u0000\u00cc\u0019\u0001\u0000\u0000\u0000"+
		"\u00cd\u00d2\u0003\u001c\u000e\u0000\u00ce\u00cf\u0005\u000f\u0000\u0000"+
		"\u00cf\u00d1\u0003\u001c\u000e\u0000\u00d0\u00ce\u0001\u0000\u0000\u0000"+
		"\u00d1\u00d4\u0001\u0000\u0000\u0000\u00d2\u00d0\u0001\u0000\u0000\u0000"+
		"\u00d2\u00d3\u0001\u0000\u0000\u0000\u00d3\u001b\u0001\u0000\u0000\u0000"+
		"\u00d4\u00d2\u0001\u0000\u0000\u0000\u00d5\u00da\u0003\u001e\u000f\u0000"+
		"\u00d6\u00d7\u0005\u0010\u0000\u0000\u00d7\u00d9\u0003\u001e\u000f\u0000"+
		"\u00d8\u00d6\u0001\u0000\u0000\u0000\u00d9\u00dc\u0001\u0000\u0000\u0000"+
		"\u00da\u00d8\u0001\u0000\u0000\u0000\u00da\u00db\u0001\u0000\u0000\u0000"+
		"\u00db\u001d\u0001\u0000\u0000\u0000\u00dc\u00da\u0001\u0000\u0000\u0000"+
		"\u00dd\u00e2\u0003 \u0010\u0000\u00de\u00df\u0007\u0000\u0000\u0000\u00df"+
		"\u00e1\u0003 \u0010\u0000\u00e0\u00de\u0001\u0000\u0000\u0000\u00e1\u00e4"+
		"\u0001\u0000\u0000\u0000\u00e2\u00e0\u0001\u0000\u0000\u0000\u00e2\u00e3"+
		"\u0001\u0000\u0000\u0000\u00e3\u001f\u0001\u0000\u0000\u0000\u00e4\u00e2"+
		"\u0001\u0000\u0000\u0000\u00e5\u00ea\u0003\"\u0011\u0000\u00e6\u00e7\u0007"+
		"\u0001\u0000\u0000\u00e7\u00e9\u0003\"\u0011\u0000\u00e8\u00e6\u0001\u0000"+
		"\u0000\u0000\u00e9\u00ec\u0001\u0000\u0000\u0000\u00ea\u00e8\u0001\u0000"+
		"\u0000\u0000\u00ea\u00eb\u0001\u0000\u0000\u0000\u00eb!\u0001\u0000\u0000"+
		"\u0000\u00ec\u00ea\u0001\u0000\u0000\u0000\u00ed\u00f2\u0003$\u0012\u0000"+
		"\u00ee\u00ef\u0007\u0002\u0000\u0000\u00ef\u00f1\u0003$\u0012\u0000\u00f0"+
		"\u00ee\u0001\u0000\u0000\u0000\u00f1\u00f4\u0001\u0000\u0000\u0000\u00f2"+
		"\u00f0\u0001\u0000\u0000\u0000\u00f2\u00f3\u0001\u0000\u0000\u0000\u00f3"+
		"#\u0001\u0000\u0000\u0000\u00f4\u00f2\u0001\u0000\u0000\u0000\u00f5\u00fa"+
		"\u0003&\u0013\u0000\u00f6\u00f7\u0007\u0003\u0000\u0000\u00f7\u00f9\u0003"+
		"&\u0013\u0000\u00f8\u00f6\u0001\u0000\u0000\u0000\u00f9\u00fc\u0001\u0000"+
		"\u0000\u0000\u00fa\u00f8\u0001\u0000\u0000\u0000\u00fa\u00fb\u0001\u0000"+
		"\u0000\u0000\u00fb%\u0001\u0000\u0000\u0000\u00fc\u00fa\u0001\u0000\u0000"+
		"\u0000\u00fd\u0102\u0003(\u0014\u0000\u00fe\u00ff\u0005\'\u0000\u0000"+
		"\u00ff\u0101\u0003&\u0013\u0000\u0100\u00fe\u0001\u0000\u0000\u0000\u0101"+
		"\u0104\u0001\u0000\u0000\u0000\u0102\u0100\u0001\u0000\u0000\u0000\u0102"+
		"\u0103\u0001\u0000\u0000\u0000\u0103\'\u0001\u0000\u0000\u0000\u0104\u0102"+
		"\u0001\u0000\u0000\u0000\u0105\u0111\u0003\n\u0005\u0000\u0106\u0111\u0003"+
		"\u0014\n\u0000\u0107\u0111\u0003\u0016\u000b\u0000\u0108\u0111\u0003\f"+
		"\u0006\u0000\u0109\u0111\u0003\u000e\u0007\u0000\u010a\u0111\u0003\u0010"+
		"\b\u0000\u010b\u0111\u0003\u0012\t\u0000\u010c\u010d\u0005(\u0000\u0000"+
		"\u010d\u010e\u0003\u0018\f\u0000\u010e\u010f\u0005)\u0000\u0000\u010f"+
		"\u0111\u0001\u0000\u0000\u0000\u0110\u0105\u0001\u0000\u0000\u0000\u0110"+
		"\u0106\u0001\u0000\u0000\u0000\u0110\u0107\u0001\u0000\u0000\u0000\u0110"+
		"\u0108\u0001\u0000\u0000\u0000\u0110\u0109\u0001\u0000\u0000\u0000\u0110"+
		"\u010a\u0001\u0000\u0000\u0000\u0110\u010b\u0001\u0000\u0000\u0000\u0110"+
		"\u010c\u0001\u0000\u0000\u0000\u0111)\u0001\u0000\u0000\u0000\u0112\u0113"+
		"\u0007\u0004\u0000\u0000\u0113+\u0001\u0000\u0000\u0000\u0114\u0115\u0007"+
		"\u0005\u0000\u0000\u0115-\u0001\u0000\u0000\u0000\u0116\u0117\u0007\u0006"+
		"\u0000\u0000\u0117/\u0001\u0000\u0000\u0000\u0118\u0119\u0003B!\u0000"+
		"\u0119\u0120\u0005(\u0000\u0000\u011a\u011b\u0003\n\u0005\u0000\u011b"+
		"\u011c\u0005\u0004\u0000\u0000\u011c\u011d\u0003\u0006\u0003\u0000\u011d"+
		"\u011f\u0001\u0000\u0000\u0000\u011e\u011a\u0001\u0000\u0000\u0000\u011f"+
		"\u0122\u0001\u0000\u0000\u0000\u0120\u011e\u0001\u0000\u0000\u0000\u0120"+
		"\u0121\u0001\u0000\u0000\u0000\u0121\u0123\u0001\u0000\u0000\u0000\u0122"+
		"\u0120\u0001\u0000\u0000\u0000\u0123\u0124\u0005)\u0000\u0000\u0124\u0125"+
		"\u0005\b\u0000\u0000\u0125\u0126\u0003\u0018\f\u0000\u01261\u0001\u0000"+
		"\u0000\u0000\u0127\u0128\u0005(\u0000\u0000\u0128\u0129\u0005.\u0000\u0000"+
		"\u0129\u012a\u0003:\u001d\u0000\u012a\u012b\u0005)\u0000\u0000\u012b\u0138"+
		"\u0001\u0000\u0000\u0000\u012c\u012d\u0005(\u0000\u0000\u012d\u012e\u0003"+
		"4\u001a\u0000\u012e\u012f\u0005\u001d\u0000\u0000\u012f\u0130\u00052\u0000"+
		"\u0000\u0130\u0131\u0005)\u0000\u0000\u0131\u0138\u0001\u0000\u0000\u0000"+
		"\u0132\u0133\u0005(\u0000\u0000\u0133\u0134\u0005/\u0000\u0000\u0134\u0135"+
		"\u0003\u0006\u0003\u0000\u0135\u0136\u0005)\u0000\u0000\u0136\u0138\u0001"+
		"\u0000\u0000\u0000\u0137\u0127\u0001\u0000\u0000\u0000\u0137\u012c\u0001"+
		"\u0000\u0000\u0000\u0137\u0132\u0001\u0000\u0000\u0000\u01383\u0001\u0000"+
		"\u0000\u0000\u0139\u013a\u0005(\u0000\u0000\u013a\u013b\u0003<\u001e\u0000"+
		"\u013b\u013c\u00050\u0000\u0000\u013c\u013d\u0003:\u001d\u0000\u013d\u013e"+
		"\u0005)\u0000\u0000\u013e\u0148\u0001\u0000\u0000\u0000\u013f\u0140\u0005"+
		"(\u0000\u0000\u0140\u0141\u0003<\u001e\u0000\u0141\u0142\u00050\u0000"+
		"\u0000\u0142\u0143\u0003:\u001d\u0000\u0143\u0144\u00050\u0000\u0000\u0144"+
		"\u0145\u0003>\u001f\u0000\u0145\u0146\u0005)\u0000\u0000\u0146\u0148\u0001"+
		"\u0000\u0000\u0000\u0147\u0139\u0001\u0000\u0000\u0000\u0147\u013f\u0001"+
		"\u0000\u0000\u0000\u01485\u0001\u0000\u0000\u0000\u0149\u014a\u0005(\u0000"+
		"\u0000\u014a\u014b\u0003:\u001d\u0000\u014b\u014c\u00050\u0000\u0000\u014c"+
		"\u014d\u0003>\u001f\u0000\u014d\u014e\u00050\u0000\u0000\u014e\u014f\u0003"+
		"4\u001a\u0000\u014f\u0150\u0005)\u0000\u0000\u01507\u0001\u0000\u0000"+
		"\u0000\u0151\u0152\u00051\u0000\u0000\u01529\u0001\u0000\u0000\u0000\u0153"+
		"\u0154\u00051\u0000\u0000\u0154;\u0001\u0000\u0000\u0000\u0155\u0156\u0005"+
		"1\u0000\u0000\u0156=\u0001\u0000\u0000\u0000\u0157\u0158\u00051\u0000"+
		"\u0000\u0158?\u0001\u0000\u0000\u0000\u0159\u015a\u00051\u0000\u0000\u015a"+
		"A\u0001\u0000\u0000\u0000\u015b\u015c\u00051\u0000\u0000\u015cC\u0001"+
		"\u0000\u0000\u0000\u001aGMZbgry\u0085\u008b\u0090\u0095\u00b2\u00b9\u00c0"+
		"\u00c8\u00d2\u00da\u00e2\u00ea\u00f2\u00fa\u0102\u0110\u0120\u0137\u0147";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}