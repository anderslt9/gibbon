// Generated from /home/anderslt/gibbon/gibbon-compiler/tests/L2-parser/L2Grammar.g4 by ANTLR 4.13.1
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
		T__17=18, T__18=19, T__19=20, VAR=21;
	public static final int
		RULE_program = 0, RULE_datatypeDecl = 1, RULE_funcDecl = 2, RULE_locatedType = 3, 
		RULE_typeScheme = 4, RULE_val = 5, RULE_express = 6, RULE_pat = 7, RULE_locExpress = 8, 
		RULE_funcVar = 9, RULE_regionVar = 10, RULE_locVar = 11, RULE_indexVar = 12, 
		RULE_typeCon = 13, RULE_dataCon = 14, RULE_locRegion = 15, RULE_concreteLoc = 16;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "datatypeDecl", "funcDecl", "locatedType", "typeScheme", "val", 
			"express", "pat", "locExpress", "funcVar", "regionVar", "locVar", "indexVar", 
			"typeCon", "dataCon", "locRegion", "concreteLoc"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'data'", "'='", "':'", "'@'", "'->'", "'['", "']'", "'let'", "'in'", 
			"'letloc'", "'letregion'", "'case'", "'of'", "'('", "')'", "'start'", 
			"'+'", "'1'", "'after'", "','"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, "VAR"
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
		public ExpressContext express() {
			return getRuleContext(ExpressContext.class,0);
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
			setState(37);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(34);
				datatypeDecl();
				}
				}
				setState(39);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(43);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(40);
					funcDecl();
					}
					} 
				}
				setState(45);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(46);
			express();
			setState(47);
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
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(49);
			match(T__0);
			setState(50);
			typeCon();
			setState(51);
			match(T__1);
			setState(61);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(52);
					dataCon();
					setState(56);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
					while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
						if ( _alt==1 ) {
							{
							{
							setState(53);
							typeCon();
							}
							} 
						}
						setState(58);
						_errHandler.sync(this);
						_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
					}
					}
					} 
				}
				setState(63);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
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
		public ExpressContext express() {
			return getRuleContext(ExpressContext.class,0);
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
			setState(64);
			funcVar();
			setState(65);
			match(T__2);
			setState(66);
			typeScheme();
			setState(67);
			funcVar();
			setState(71);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==VAR) {
				{
				{
				setState(68);
				match(VAR);
				}
				}
				setState(73);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(74);
			match(T__1);
			setState(75);
			express();
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
			setState(77);
			typeCon();
			setState(78);
			match(T__3);
			setState(79);
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
			setState(86);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(81);
					locatedType();
					setState(82);
					match(T__4);
					}
					} 
				}
				setState(88);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			}
			setState(89);
			locatedType();
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
		public ValContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_val; }
	}

	public final ValContext val() throws RecognitionException {
		ValContext _localctx = new ValContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_val);
		try {
			setState(93);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case VAR:
				enterOuterAlt(_localctx, 1);
				{
				setState(91);
				match(VAR);
				}
				break;
			case T__13:
				enterOuterAlt(_localctx, 2);
				{
				setState(92);
				concreteLoc();
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
	public static class ExpressContext extends ParserRuleContext {
		public List<ValContext> val() {
			return getRuleContexts(ValContext.class);
		}
		public ValContext val(int i) {
			return getRuleContext(ValContext.class,i);
		}
		public FuncVarContext funcVar() {
			return getRuleContext(FuncVarContext.class,0);
		}
		public List<LocRegionContext> locRegion() {
			return getRuleContexts(LocRegionContext.class);
		}
		public LocRegionContext locRegion(int i) {
			return getRuleContext(LocRegionContext.class,i);
		}
		public DataConContext dataCon() {
			return getRuleContext(DataConContext.class,0);
		}
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public LocatedTypeContext locatedType() {
			return getRuleContext(LocatedTypeContext.class,0);
		}
		public List<ExpressContext> express() {
			return getRuleContexts(ExpressContext.class);
		}
		public ExpressContext express(int i) {
			return getRuleContext(ExpressContext.class,i);
		}
		public LocExpressContext locExpress() {
			return getRuleContext(LocExpressContext.class,0);
		}
		public RegionVarContext regionVar() {
			return getRuleContext(RegionVarContext.class,0);
		}
		public List<PatContext> pat() {
			return getRuleContexts(PatContext.class);
		}
		public PatContext pat(int i) {
			return getRuleContext(PatContext.class,i);
		}
		public ExpressContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_express; }
	}

	public final ExpressContext express() throws RecognitionException {
		ExpressContext _localctx = new ExpressContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_express);
		int _la;
		try {
			int _alt;
			setState(149);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,11,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(95);
				val();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(96);
				funcVar();
				setState(97);
				match(T__5);
				setState(101);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__13) {
					{
					{
					setState(98);
					locRegion();
					}
					}
					setState(103);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(104);
				match(T__6);
				setState(108);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(105);
						val();
						}
						} 
					}
					setState(110);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
				}
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(111);
				dataCon();
				setState(112);
				locRegion();
				setState(116);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(113);
						val();
						}
						} 
					}
					setState(118);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
				}
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(119);
				match(T__7);
				setState(120);
				match(VAR);
				setState(121);
				match(T__2);
				setState(122);
				locatedType();
				setState(123);
				match(T__1);
				setState(124);
				express();
				setState(125);
				match(T__8);
				setState(126);
				express();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(128);
				match(T__9);
				setState(129);
				locRegion();
				setState(130);
				match(T__1);
				setState(131);
				locExpress();
				setState(132);
				match(T__8);
				setState(133);
				express();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(135);
				match(T__10);
				setState(136);
				regionVar();
				setState(137);
				match(T__8);
				setState(138);
				express();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(140);
				match(T__11);
				setState(141);
				val();
				setState(142);
				match(T__12);
				setState(146);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,10,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(143);
						pat();
						}
						} 
					}
					setState(148);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,10,_ctx);
				}
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
	public static class PatContext extends ParserRuleContext {
		public DataConContext dataCon() {
			return getRuleContext(DataConContext.class,0);
		}
		public ExpressContext express() {
			return getRuleContext(ExpressContext.class,0);
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
		enterRule(_localctx, 14, RULE_pat);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(151);
			dataCon();
			setState(152);
			match(T__13);
			setState(159);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__13 || _la==VAR) {
				{
				{
				setState(153);
				val();
				setState(154);
				match(T__2);
				setState(155);
				locatedType();
				}
				}
				setState(161);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(162);
			match(T__14);
			setState(163);
			match(T__4);
			setState(164);
			express();
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
		enterRule(_localctx, 16, RULE_locExpress);
		try {
			setState(182);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(166);
				match(T__13);
				setState(167);
				match(T__15);
				setState(168);
				regionVar();
				setState(169);
				match(T__14);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(171);
				match(T__13);
				setState(172);
				locRegion();
				setState(173);
				match(T__16);
				setState(174);
				match(T__17);
				setState(175);
				match(T__14);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(177);
				match(T__13);
				setState(178);
				match(T__18);
				setState(179);
				locatedType();
				setState(180);
				match(T__14);
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
	public static class FuncVarContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(L2GrammarParser.VAR, 0); }
		public FuncVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcVar; }
	}

	public final FuncVarContext funcVar() throws RecognitionException {
		FuncVarContext _localctx = new FuncVarContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_funcVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(184);
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
		enterRule(_localctx, 20, RULE_regionVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(186);
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
		enterRule(_localctx, 22, RULE_locVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(188);
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
		enterRule(_localctx, 24, RULE_indexVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(190);
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
		enterRule(_localctx, 26, RULE_typeCon);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(192);
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
		enterRule(_localctx, 28, RULE_dataCon);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(194);
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
		enterRule(_localctx, 30, RULE_locRegion);
		try {
			setState(210);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(196);
				match(T__13);
				setState(197);
				locVar();
				setState(198);
				match(T__19);
				setState(199);
				regionVar();
				setState(200);
				match(T__14);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(202);
				match(T__13);
				setState(203);
				locVar();
				setState(204);
				match(T__19);
				setState(205);
				regionVar();
				setState(206);
				match(T__19);
				setState(207);
				indexVar();
				setState(208);
				match(T__14);
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
		enterRule(_localctx, 32, RULE_concreteLoc);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(212);
			match(T__13);
			setState(213);
			regionVar();
			setState(214);
			match(T__19);
			setState(215);
			indexVar();
			setState(216);
			match(T__19);
			setState(217);
			locRegion();
			setState(218);
			match(T__14);
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
		"\u0004\u0001\u0015\u00dd\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004"+
		"\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007"+
		"\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b"+
		"\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007"+
		"\u000f\u0002\u0010\u0007\u0010\u0001\u0000\u0005\u0000$\b\u0000\n\u0000"+
		"\f\u0000\'\t\u0000\u0001\u0000\u0005\u0000*\b\u0000\n\u0000\f\u0000-\t"+
		"\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0005\u00017\b\u0001\n\u0001\f\u0001:\t"+
		"\u0001\u0005\u0001<\b\u0001\n\u0001\f\u0001?\t\u0001\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0005\u0002F\b\u0002\n\u0002"+
		"\f\u0002I\t\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0003\u0001"+
		"\u0003\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0005"+
		"\u0004U\b\u0004\n\u0004\f\u0004X\t\u0004\u0001\u0004\u0001\u0004\u0001"+
		"\u0005\u0001\u0005\u0003\u0005^\b\u0005\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0005\u0006d\b\u0006\n\u0006\f\u0006g\t\u0006\u0001"+
		"\u0006\u0001\u0006\u0005\u0006k\b\u0006\n\u0006\f\u0006n\t\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0005\u0006s\b\u0006\n\u0006\f\u0006v\t"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0005\u0006\u0091\b\u0006\n\u0006\f\u0006\u0094\t\u0006"+
		"\u0003\u0006\u0096\b\u0006\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0005\u0007\u009e\b\u0007\n\u0007\f\u0007\u00a1"+
		"\t\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u00b7\b\b\u0001\t\u0001"+
		"\t\u0001\n\u0001\n\u0001\u000b\u0001\u000b\u0001\f\u0001\f\u0001\r\u0001"+
		"\r\u0001\u000e\u0001\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0003\u000f\u00d3\b\u000f"+
		"\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010"+
		"\u0001\u0010\u0001\u0010\u0001\u0010\u0000\u0000\u0011\u0000\u0002\u0004"+
		"\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e \u0000"+
		"\u0000\u00e0\u0000%\u0001\u0000\u0000\u0000\u00021\u0001\u0000\u0000\u0000"+
		"\u0004@\u0001\u0000\u0000\u0000\u0006M\u0001\u0000\u0000\u0000\bV\u0001"+
		"\u0000\u0000\u0000\n]\u0001\u0000\u0000\u0000\f\u0095\u0001\u0000\u0000"+
		"\u0000\u000e\u0097\u0001\u0000\u0000\u0000\u0010\u00b6\u0001\u0000\u0000"+
		"\u0000\u0012\u00b8\u0001\u0000\u0000\u0000\u0014\u00ba\u0001\u0000\u0000"+
		"\u0000\u0016\u00bc\u0001\u0000\u0000\u0000\u0018\u00be\u0001\u0000\u0000"+
		"\u0000\u001a\u00c0\u0001\u0000\u0000\u0000\u001c\u00c2\u0001\u0000\u0000"+
		"\u0000\u001e\u00d2\u0001\u0000\u0000\u0000 \u00d4\u0001\u0000\u0000\u0000"+
		"\"$\u0003\u0002\u0001\u0000#\"\u0001\u0000\u0000\u0000$\'\u0001\u0000"+
		"\u0000\u0000%#\u0001\u0000\u0000\u0000%&\u0001\u0000\u0000\u0000&+\u0001"+
		"\u0000\u0000\u0000\'%\u0001\u0000\u0000\u0000(*\u0003\u0004\u0002\u0000"+
		")(\u0001\u0000\u0000\u0000*-\u0001\u0000\u0000\u0000+)\u0001\u0000\u0000"+
		"\u0000+,\u0001\u0000\u0000\u0000,.\u0001\u0000\u0000\u0000-+\u0001\u0000"+
		"\u0000\u0000./\u0003\f\u0006\u0000/0\u0005\u0000\u0000\u00010\u0001\u0001"+
		"\u0000\u0000\u000012\u0005\u0001\u0000\u000023\u0003\u001a\r\u00003=\u0005"+
		"\u0002\u0000\u000048\u0003\u001c\u000e\u000057\u0003\u001a\r\u000065\u0001"+
		"\u0000\u0000\u00007:\u0001\u0000\u0000\u000086\u0001\u0000\u0000\u0000"+
		"89\u0001\u0000\u0000\u00009<\u0001\u0000\u0000\u0000:8\u0001\u0000\u0000"+
		"\u0000;4\u0001\u0000\u0000\u0000<?\u0001\u0000\u0000\u0000=;\u0001\u0000"+
		"\u0000\u0000=>\u0001\u0000\u0000\u0000>\u0003\u0001\u0000\u0000\u0000"+
		"?=\u0001\u0000\u0000\u0000@A\u0003\u0012\t\u0000AB\u0005\u0003\u0000\u0000"+
		"BC\u0003\b\u0004\u0000CG\u0003\u0012\t\u0000DF\u0005\u0015\u0000\u0000"+
		"ED\u0001\u0000\u0000\u0000FI\u0001\u0000\u0000\u0000GE\u0001\u0000\u0000"+
		"\u0000GH\u0001\u0000\u0000\u0000HJ\u0001\u0000\u0000\u0000IG\u0001\u0000"+
		"\u0000\u0000JK\u0005\u0002\u0000\u0000KL\u0003\f\u0006\u0000L\u0005\u0001"+
		"\u0000\u0000\u0000MN\u0003\u001a\r\u0000NO\u0005\u0004\u0000\u0000OP\u0003"+
		"\u001e\u000f\u0000P\u0007\u0001\u0000\u0000\u0000QR\u0003\u0006\u0003"+
		"\u0000RS\u0005\u0005\u0000\u0000SU\u0001\u0000\u0000\u0000TQ\u0001\u0000"+
		"\u0000\u0000UX\u0001\u0000\u0000\u0000VT\u0001\u0000\u0000\u0000VW\u0001"+
		"\u0000\u0000\u0000WY\u0001\u0000\u0000\u0000XV\u0001\u0000\u0000\u0000"+
		"YZ\u0003\u0006\u0003\u0000Z\t\u0001\u0000\u0000\u0000[^\u0005\u0015\u0000"+
		"\u0000\\^\u0003 \u0010\u0000][\u0001\u0000\u0000\u0000]\\\u0001\u0000"+
		"\u0000\u0000^\u000b\u0001\u0000\u0000\u0000_\u0096\u0003\n\u0005\u0000"+
		"`a\u0003\u0012\t\u0000ae\u0005\u0006\u0000\u0000bd\u0003\u001e\u000f\u0000"+
		"cb\u0001\u0000\u0000\u0000dg\u0001\u0000\u0000\u0000ec\u0001\u0000\u0000"+
		"\u0000ef\u0001\u0000\u0000\u0000fh\u0001\u0000\u0000\u0000ge\u0001\u0000"+
		"\u0000\u0000hl\u0005\u0007\u0000\u0000ik\u0003\n\u0005\u0000ji\u0001\u0000"+
		"\u0000\u0000kn\u0001\u0000\u0000\u0000lj\u0001\u0000\u0000\u0000lm\u0001"+
		"\u0000\u0000\u0000m\u0096\u0001\u0000\u0000\u0000nl\u0001\u0000\u0000"+
		"\u0000op\u0003\u001c\u000e\u0000pt\u0003\u001e\u000f\u0000qs\u0003\n\u0005"+
		"\u0000rq\u0001\u0000\u0000\u0000sv\u0001\u0000\u0000\u0000tr\u0001\u0000"+
		"\u0000\u0000tu\u0001\u0000\u0000\u0000u\u0096\u0001\u0000\u0000\u0000"+
		"vt\u0001\u0000\u0000\u0000wx\u0005\b\u0000\u0000xy\u0005\u0015\u0000\u0000"+
		"yz\u0005\u0003\u0000\u0000z{\u0003\u0006\u0003\u0000{|\u0005\u0002\u0000"+
		"\u0000|}\u0003\f\u0006\u0000}~\u0005\t\u0000\u0000~\u007f\u0003\f\u0006"+
		"\u0000\u007f\u0096\u0001\u0000\u0000\u0000\u0080\u0081\u0005\n\u0000\u0000"+
		"\u0081\u0082\u0003\u001e\u000f\u0000\u0082\u0083\u0005\u0002\u0000\u0000"+
		"\u0083\u0084\u0003\u0010\b\u0000\u0084\u0085\u0005\t\u0000\u0000\u0085"+
		"\u0086\u0003\f\u0006\u0000\u0086\u0096\u0001\u0000\u0000\u0000\u0087\u0088"+
		"\u0005\u000b\u0000\u0000\u0088\u0089\u0003\u0014\n\u0000\u0089\u008a\u0005"+
		"\t\u0000\u0000\u008a\u008b\u0003\f\u0006\u0000\u008b\u0096\u0001\u0000"+
		"\u0000\u0000\u008c\u008d\u0005\f\u0000\u0000\u008d\u008e\u0003\n\u0005"+
		"\u0000\u008e\u0092\u0005\r\u0000\u0000\u008f\u0091\u0003\u000e\u0007\u0000"+
		"\u0090\u008f\u0001\u0000\u0000\u0000\u0091\u0094\u0001\u0000\u0000\u0000"+
		"\u0092\u0090\u0001\u0000\u0000\u0000\u0092\u0093\u0001\u0000\u0000\u0000"+
		"\u0093\u0096\u0001\u0000\u0000\u0000\u0094\u0092\u0001\u0000\u0000\u0000"+
		"\u0095_\u0001\u0000\u0000\u0000\u0095`\u0001\u0000\u0000\u0000\u0095o"+
		"\u0001\u0000\u0000\u0000\u0095w\u0001\u0000\u0000\u0000\u0095\u0080\u0001"+
		"\u0000\u0000\u0000\u0095\u0087\u0001\u0000\u0000\u0000\u0095\u008c\u0001"+
		"\u0000\u0000\u0000\u0096\r\u0001\u0000\u0000\u0000\u0097\u0098\u0003\u001c"+
		"\u000e\u0000\u0098\u009f\u0005\u000e\u0000\u0000\u0099\u009a\u0003\n\u0005"+
		"\u0000\u009a\u009b\u0005\u0003\u0000\u0000\u009b\u009c\u0003\u0006\u0003"+
		"\u0000\u009c\u009e\u0001\u0000\u0000\u0000\u009d\u0099\u0001\u0000\u0000"+
		"\u0000\u009e\u00a1\u0001\u0000\u0000\u0000\u009f\u009d\u0001\u0000\u0000"+
		"\u0000\u009f\u00a0\u0001\u0000\u0000\u0000\u00a0\u00a2\u0001\u0000\u0000"+
		"\u0000\u00a1\u009f\u0001\u0000\u0000\u0000\u00a2\u00a3\u0005\u000f\u0000"+
		"\u0000\u00a3\u00a4\u0005\u0005\u0000\u0000\u00a4\u00a5\u0003\f\u0006\u0000"+
		"\u00a5\u000f\u0001\u0000\u0000\u0000\u00a6\u00a7\u0005\u000e\u0000\u0000"+
		"\u00a7\u00a8\u0005\u0010\u0000\u0000\u00a8\u00a9\u0003\u0014\n\u0000\u00a9"+
		"\u00aa\u0005\u000f\u0000\u0000\u00aa\u00b7\u0001\u0000\u0000\u0000\u00ab"+
		"\u00ac\u0005\u000e\u0000\u0000\u00ac\u00ad\u0003\u001e\u000f\u0000\u00ad"+
		"\u00ae\u0005\u0011\u0000\u0000\u00ae\u00af\u0005\u0012\u0000\u0000\u00af"+
		"\u00b0\u0005\u000f\u0000\u0000\u00b0\u00b7\u0001\u0000\u0000\u0000\u00b1"+
		"\u00b2\u0005\u000e\u0000\u0000\u00b2\u00b3\u0005\u0013\u0000\u0000\u00b3"+
		"\u00b4\u0003\u0006\u0003\u0000\u00b4\u00b5\u0005\u000f\u0000\u0000\u00b5"+
		"\u00b7\u0001\u0000\u0000\u0000\u00b6\u00a6\u0001\u0000\u0000\u0000\u00b6"+
		"\u00ab\u0001\u0000\u0000\u0000\u00b6\u00b1\u0001\u0000\u0000\u0000\u00b7"+
		"\u0011\u0001\u0000\u0000\u0000\u00b8\u00b9\u0005\u0015\u0000\u0000\u00b9"+
		"\u0013\u0001\u0000\u0000\u0000\u00ba\u00bb\u0005\u0015\u0000\u0000\u00bb"+
		"\u0015\u0001\u0000\u0000\u0000\u00bc\u00bd\u0005\u0015\u0000\u0000\u00bd"+
		"\u0017\u0001\u0000\u0000\u0000\u00be\u00bf\u0005\u0015\u0000\u0000\u00bf"+
		"\u0019\u0001\u0000\u0000\u0000\u00c0\u00c1\u0005\u0015\u0000\u0000\u00c1"+
		"\u001b\u0001\u0000\u0000\u0000\u00c2\u00c3\u0005\u0015\u0000\u0000\u00c3"+
		"\u001d\u0001\u0000\u0000\u0000\u00c4\u00c5\u0005\u000e\u0000\u0000\u00c5"+
		"\u00c6\u0003\u0016\u000b\u0000\u00c6\u00c7\u0005\u0014\u0000\u0000\u00c7"+
		"\u00c8\u0003\u0014\n\u0000\u00c8\u00c9\u0005\u000f\u0000\u0000\u00c9\u00d3"+
		"\u0001\u0000\u0000\u0000\u00ca\u00cb\u0005\u000e\u0000\u0000\u00cb\u00cc"+
		"\u0003\u0016\u000b\u0000\u00cc\u00cd\u0005\u0014\u0000\u0000\u00cd\u00ce"+
		"\u0003\u0014\n\u0000\u00ce\u00cf\u0005\u0014\u0000\u0000\u00cf\u00d0\u0003"+
		"\u0018\f\u0000\u00d0\u00d1\u0005\u000f\u0000\u0000\u00d1\u00d3\u0001\u0000"+
		"\u0000\u0000\u00d2\u00c4\u0001\u0000\u0000\u0000\u00d2\u00ca\u0001\u0000"+
		"\u0000\u0000\u00d3\u001f\u0001\u0000\u0000\u0000\u00d4\u00d5\u0005\u000e"+
		"\u0000\u0000\u00d5\u00d6\u0003\u0014\n\u0000\u00d6\u00d7\u0005\u0014\u0000"+
		"\u0000\u00d7\u00d8\u0003\u0018\f\u0000\u00d8\u00d9\u0005\u0014\u0000\u0000"+
		"\u00d9\u00da\u0003\u001e\u000f\u0000\u00da\u00db\u0005\u000f\u0000\u0000"+
		"\u00db!\u0001\u0000\u0000\u0000\u000f%+8=GV]elt\u0092\u0095\u009f\u00b6"+
		"\u00d2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}