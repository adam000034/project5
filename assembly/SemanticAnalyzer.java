import java.util.Vector;

public class SemanticAnalyzer implements ASTVisitor {

    class TypeClass {
        Type type_;
        AATExpression value_;
        
        public TypeClass(Type type, AATExpression value) {
            type_ = type;
            value_ = value;
        }
        
        public Type type() {
            return type_;
        }
        
        public AATExpression value() {
            return value_;
        }
        
        public void settype(Type type) {
            type_ = type;
        }
    }
    
    private VariableEnvironment variableEnv;
    private FunctionEnvironment functionEnv;
    private TypeEnvironment typeEnv;
    
    private boolean addFormalsToVarEnv;
    
    private int offset;    
    private int formalOffset;   //for formals: negative
    private String currentFunctionName;
    private AATBuildTree bt;

    public SemanticAnalyzer() {
        variableEnv = new VariableEnvironment();
        functionEnv = new FunctionEnvironment();
        functionEnv.addBuiltinFunctions();
        typeEnv = new TypeEnvironment();
        addFormalsToVarEnv = false;
        bt = new AATBuildTree();
        offset = 0;
        formalOffset = (-1 * MachineDependent.WORDSIZE);
    }
    
    /**
     * Checks if size element of array is an integer. Gives error if not. If it is an integer, adds
     * type to type environment if it does not exist.
     * 
     * @param newarrayexpression
     * @return Integer instance if error, instance of ArrayType object if not
     */
    public Object VisitNewArrayExpression(ASTNewArrayExpression newarrayexpression) {
        TypeClass sizetc = (TypeClass) newarrayexpression.elements().Accept(this);
        Type size = sizetc.type(); 
                
         if (size != IntegerType.instance()) {
             CompError.message(newarrayexpression.line(), "Array size must be an integer.");
             return new TypeClass(IntegerType.instance(), null);
         } else {
             //type is base type (i.e. int for int[][])
             Type arrtyp = CheckType(newarrayexpression.type(), newarrayexpression.arraydimension(), newarrayexpression.line());
             return new TypeClass(arrtyp, bt.allocate(bt.operatorExpression(sizetc.value(), bt.constantExpression(MachineDependent.WORDSIZE), AATOperator.MULTIPLY))); //TODO: Make sure this is in bytes, not words
         }        
     }  /* DONE */
    
    /**
     * Adds all brackets to base type for array type. 
     * 
     * If entry does not exist in Type Environment,
     * recursively calls itself with an array dimension - 1. This ensures that all array types with
     * same base but less array dimension will be added if they are missing. It then creates a new
     * ArrayType instance by passing in the Type instance in from the ArrayType with one less array
     * dimensionality into the ArrayType constructor. This new ArrayType instance is inserted into
     * the Type Environment with the key <basetype + ("[]" * arraydimension)>, and is then returned.
     * 
     * However, if the entry exists, it is returned. 
     * 
     * @param type - base type of array
     * @param arraydimension
     * @param linenum
     * @return ArrayType entry for the specific array with base type "type" and the provided array
     * dimensionality.
     */
    public Type CheckType(String type, int arraydimension, int linenum) {
        //////System.out.println("CheckType()");
        if (typeEnv.find(type) == null) {
            CompError.message(linenum, "Base Type does not exist.");
            return IntegerType.instance();
        }
        String thisType = type;
        for (int i = 0; i < arraydimension; i++) {
            thisType += "[]";
        }
        //I see potential problems here
        //ArrayType entry = new ArrayType(typeEnv.find(thisType));
        Type entry = (Type) typeEnv.find(thisType);

        //if type is not in type environment, recursive call to add it
        //will add previous dimensional ones if not in environment either
        if (entry == null && arraydimension != 0) {
            Type prevDimTypeEntry = CheckType(type, arraydimension - 1, linenum);
            ArrayType arrTypeInstance = new ArrayType(prevDimTypeEntry);
            typeEnv.insert(thisType, arrTypeInstance);
            return arrTypeInstance;
        } else {
            return entry;
        }
    }   /* DONE */
    
    /**
     * Visits each class.
     * 
     * @param classes
     */
    public Object VisitClasses(ASTClasses classes){
        for (int i = 0; i <classes.size(); i++) {
            classes.elementAt(i).Accept(this);
        }
        return null;
    }   /* DONE */
    
    /**
     * Checks that types of left-hand side and right-hand side of the Assignment
     * statement are the same.
     * 
     * @param assignstatement
     */
    public Object VisitAssignmentStatement(ASTAssignmentStatement assignstatement) {
        TypeClass lhstc = (TypeClass) assignstatement.variable().Accept(this);
        TypeClass rhstc = (TypeClass) assignstatement.value().Accept(this);
        Type lhs = lhstc.type();
        Type rhs = rhstc.type();
        
        if (lhs != rhs) {
            CompError.message(assignstatement.line(), "Lefthand side and righthand "
                    + "side of an assignment statement must match.");
            //////System.out.println("lhs: " + lhs + " rhs: " + rhs);
        }
        return bt.assignmentStatement(lhstc.value(), rhstc.value());
    }   /* DONE */

    
    /**
     * Checks base variable and checks if index is an integer.
     * 
     * @param arrayvariable
     */
    public Object VisitArrayVariable(ASTArrayVariable arrayvariable) {
        //////System.out.println("VisitArrayVariable() LINE: "+arrayvariable.line() + " BASE: "+ arrayvariable.base());
        TypeClass typetc = (TypeClass) arrayvariable.base().Accept(this);
        TypeClass typeOfIndextc = (TypeClass) arrayvariable.index().Accept(this);
        Type type = typetc.type();
        Type typeOfIndex = typeOfIndextc.type();
        
        if (typeOfIndex != IntegerType.instance()) {
            CompError.message(arrayvariable.line(), "Index of an array must be of "
                    + "type integer.");
            return new TypeClass(IntegerType.instance(), null);
        }
        if (! type.getClass().equals(ArrayType.class)) {
            CompError.message(arrayvariable.line(), "Variable is not an array.");
            return new TypeClass(IntegerType.instance(), null);
        }
        return new TypeClass(((ArrayType) type).type(), bt.arrayVariable(typetc.value(), typeOfIndextc.value(), MachineDependent.WORDSIZE));
    }   /* DONE */
    
    /**
     * Returns the instance of BooleanType.
     * 
     * @param booliteral
     * @return BooleanType's instance
     */
    public Object VisitBooleanLiteral(ASTBooleanLiteral boolliteral) {
        if (boolliteral.value()) {
            return new TypeClass(BooleanType.instance(), bt.constantExpression(1));
        } else {
            return new TypeClass(BooleanType.instance(), bt.constantExpression(0));
        }
    }   /* DONE */
    
    /**
     * Creates a new variable environment for the class type, goes through each
     * variable definition and inserts it into class's variable environment, then
     * creates new ClassType and inserts it into global type environment.
     * 
     * @param asclass
     * @return IntegerType instance if error, else ClassType
     */
    public Object VisitClass(ASTClass asclass) {
        ResetOffsets();
        
        if (typeEnv.find(asclass.name()) != null) {
            CompError.message(asclass.line(), "Cannot have classes with the same name.");
            return IntegerType.instance();
        }
        
        VariableEnvironment variables = new VariableEnvironment();
        ASTInstanceVariableDefs variabledefs = asclass.variabledefs();
        Type type;
        if (variabledefs != null) {
            variabledefs.Accept(this);
            //Go through each variable definition and insert it into class's 
            //variable environment
            ASTInstanceVariableDef vardef;
            for (int i = 0; i < variabledefs.size(); i++) {
                vardef = variabledefs.elementAt(i);
                type = CheckType(vardef.type(), vardef.arraydimension(), vardef.line());
                //If there is a variable def of same name already in the class's variable enviro,
                //give an error
                if (variables.find(vardef.name()) != null) {
                    CompError.message(vardef.line(), "Cannot have 2 instance variables"
                            + "of the same name within the same class.");
                    return IntegerType.instance();
                }
                variables.insert(vardef.name(), new VariableEntry(type, IncrementOffset()));
            }
        }
        ClassType classType = new ClassType(variables);
        //Create new Type entry for class
        typeEnv.insert(asclass.name(), classType);
        //functionEnv.insert(asclass.name(), new FunctionEntry(classType, new Vector<Type>()));
        return classType;
    }   /* DONE */
    
    /**
     * Checks to make sure type of definition is in typeEnv. if not, error
     * 
     * @param variabledef
     * @return IntegerType instance if type is n
     */
    public Object VisitInstanceVariableDef(ASTInstanceVariableDef variabledef)
    {
        //////System.out.println("VisitInstanceVariableDef()");
        return CheckType(variabledef.type(), variabledef.arraydimension(), variabledef.line());
    }   /* DONE */
    
    /**
     * Calls accept on base variable.
     * 
     * @param classvar
     */
    public Object VisitClassVariable(ASTClassVariable classvar){
        //System.out.println("VisitClassVariable() LINE: "+classvar.line() + " BASE: "+ classvar.base());
        //ClassType classType = (ClassType) classvar.base().Accept(this);
        TypeClass typetc = (TypeClass) classvar.base().Accept(this);
        Type type = typetc.type();

        /*if (type.getClass().equals(ArrayType.class)) {
            while (type.getClass().equals(ArrayType.class)) {
                type = ((ArrayType) type).type();
            }
        }*/
        if (!type.getClass().equals(ClassType.class)) {
            CompError.message(classvar.line(), "Base is not a class type.");
            return new TypeClass(IntegerType.instance(), null);
        }
        ClassType classType = (ClassType) type;
        //get class type object
        //look into variable environment
        //does it have the variable? if not, error
        VariableEntry varEntry = classType.variables().find(classvar.variable());
        //System.out.println("  VisitClassVariable(): "+classvar.variable());
        if (varEntry == null) {
            CompError.message(classvar.line(), "Class type does not have variable " + classvar.variable());
            return new TypeClass(IntegerType.instance(), null);
        }
        return new TypeClass(varEntry.type(), bt.classVariable(typetc.value(), varEntry.offset()));
    }   /* DONE */
    
    /**
     * Begins new scope on the variable environment, calls Accept() of 
     * the iterator initialization statement, the test, the increment 
     * statement, and the body, then ends scope.
     * 
     * @param forstatement
     */
    public Object VisitForStatement(ASTForStatement forstatement) {
        variableEnv.beginScope();
        AATStatement init = (AATStatement) forstatement.initialize().Accept(this);
        TypeClass test = (TypeClass) forstatement.test().Accept(this);
        AATStatement increment = (AATStatement) forstatement.increment().Accept(this);
        AATStatement body = (AATStatement) forstatement.body().Accept(this);
        variableEnv.endScope();
        return bt.forStatement(init, test.value(), increment, body);
    }   /* DONE */
    
    /**
     * Returns null. There is nothing to be done for an empty statement.
     * 
     * @param emptystate
     */
    public Object VisitEmptyStatement(ASTEmptyStatement emptystate) {
        return bt.emptyStatement();
    }   /* DONE */
        
    /**
     * Begins new scope on the variable environment, calls Accept() of 
     * the test and the body, then ends scope.
     * 
     * @param dowhile
     */
    public Object VisitDoWhileStatement(ASTDoWhileStatement dowhile) {
        variableEnv.beginScope();
        TypeClass test = (TypeClass) dowhile.test().Accept(this);
        AATStatement body = (AATStatement) dowhile.body().Accept(this);
        variableEnv.endScope();
        return bt.dowhileStatement(test.value(), body);
    }   /* DONE */
    
    /**
     * Checks that formal is correct by calling helper method CheckType()
     * 
     * @param formal
     * @return Type of formal. If Integer, may be error.
     */
    public Object VisitFormal(ASTFormal formal) {
        Type type = CheckType(formal.type(), formal.arraydimension(), formal.line());
               
        //Add formals to variable environment if option is set to true
        if (addFormalsToVarEnv) {
            if (variableEnv.find(formal.name()) != null) {
                CompError.message(formal.line(), "Cannot have 2 formals"
                        + "of the same name for the same function.");
                return IntegerType.instance();
            }
            variableEnv.insert(formal.name(), new VariableEntry(type, DecrementFormalOffset()));
        }     
        return type;
    }   /* DONE */
    
    /**
     * Visits each formal.
     * 
     * @param formals
     */
    public Object VisitFormals(ASTFormals formals) {
        if (formals != null) {  /* also had formals.size() == 0 but removed it */
            for (int i=0; i<formals.size(); i++) {
                formals.elementAt(i).Accept(this);
            }
        }
        return null;
    }   /* DONE */
    
    /**
     * If prototype already exists, compares prototype and function
     * - compares number of formals
     * - compares types of each formal
     * - compares return types
     * Else,
     * - checks if return type exists 
     * - setup: sets hasPrototype to false so that function entry will be added later
     * 
     * Begins a new scope in variable environment.
     * Adds formals to variable environment: addFormalsToVarEnv = true
     * Adds function entry to function environment if no prototype.
     * Sets addFormalsToVarEnv to false.
     * Analyzes body of function.
     * Ends scope.
     * 
     * @param function
     */
    public Object VisitFunction(ASTFunction function) {
        ResetOffsets();
        currentFunctionName = function.name();
        boolean hasPrototype;

        //Analyze formal parameters & return type
        FunctionEntry funcEntry = functionEnv.find(function.name());
        //Check against prototype (if there is one), 
        if (funcEntry != null) {
            hasPrototype = true;
            Vector<Type> funcEntryFormals = funcEntry.formals();      //list of Type objects of function prototype formals
            ASTFormals functionFormals = function.formals();    //list of ASTFormal objects
            
            //- Is the return type the same?
            Type type = typeEnv.find(function.type());
            if (! funcEntry.result().equals(type)) {
                CompError.message(function.line(), "A function's return type must match "
                        + "with its function prototype's return type.");
            }
            
            //- Check number of formals
            if (funcEntryFormals.size() < functionFormals.size()) {
                CompError.message(function.line(), "A function's formal parameters must match "
                        + "with its function prototype's formal parameters. - Too many formals.");
            } else if (funcEntryFormals.size() > functionFormals.size()) {
                CompError.message(function.line(), "A function's formal parameters must match "
                        + "with its function prototype's formal parameters. - Too few formals.");
            } else {
                //- Are the formals the same? -- type
                for (int i = 0; i < funcEntryFormals.size(); i++) {
                    Type functionFormalType = CheckType(functionFormals.elementAt(i).type(), 
                            functionFormals.elementAt(i).arraydimension(), functionFormals.elementAt(i).line());
                    //Check if type doesn't equal it's counterpart in its function prototype
                    //-- don't have to check if type exists because prototype would have done this, just
                    //   comparing types
                    if (! (funcEntryFormals.elementAt(i).equals(functionFormalType))) {
                        CompError.message(function.line(), "A function's formal parameters must match "
                                + "with its function prototype's formal parameters. - Different formal types.");
                    }
                }
            }
        } else {        //or add function entry to function environment (if no prototype)
            hasPrototype = false;
            //check if return type exists
            if (typeEnv.find(function.type()) == null) {
                CompError.message(function.line(), "The return type of the funciton is not a valid type.");
            }
            //don't add function entry to function enviro yet... 
            //Add it after VisitFormal adds possible new dimension array types to typeEnv
        }
                
        //Begin a new scope in the variable environment
        variableEnv.beginScope();       
        
        //VisitFormal should add formals to variable environment in this case.
        addFormalsToVarEnv = true;

        Vector<Type> params = new Vector<Type>();
        if (function.formals() != null) {
            Type paramType;
            for (int i=0; i < function.formals().size(); i++) {
                //Add formal parameters to the variable environment  
                paramType = (Type) function.formals().elementAt(i).Accept(this);
                params.add(paramType);
            }
        }
        //Add function entry to function environment if there is no prototype
        if (!hasPrototype) {
            Type type = ReturnTypeHelper(function.type(), function.line());
            functionEnv.insert(function.name(), new FunctionEntry(type, params, new Label(function.name()), new Label(function.name())));
            funcEntry = functionEnv.find(function.name());
        }
        
        addFormalsToVarEnv = false;
        Type typeofreturn = typeEnv.find(function.type());
        VariableEntry varentry = new VariableEntry(typeofreturn);
        //assumming that there will be no more than one "return" at a time
        variableEnv.insert("return", varentry);
        //Analyze the body of the function, using modified variable environment
        AATStatement body = (AATStatement) function.body().Accept(this);
        //End current scope in variable environment
        variableEnv.endScope();

        return bt.functionDefinition(body, offset, funcEntry.startlabel(), funcEntry.endlabel());
        //TODO: check body and framesize (offset)
    }   /* DONE */
    
    public Object VisitFunctionCallExpression(ASTFunctionCallExpression callexpression) {
        ////System.out.println("VisitFunctionCallExpression() LINE: " + callexpression.line());
        //check to see if function exists in func environment
        FunctionEntry funcEntry = functionEnv.find(callexpression.name());
        if (funcEntry == null) {
            CompError.message(callexpression.line(), "Function " + callexpression.name() + " is not defined in this " +
                              "scope");
            return new TypeClass(IntegerType.instance(), null);
        }
        if (callexpression.size() < funcEntry.formals().size()) {
            CompError.message(callexpression.line(), "Function call has too few actual parameters.");
            
            return new TypeClass(IntegerType.instance(), null);
        }
        if (callexpression.size() > funcEntry.formals().size()) {
            CompError.message(callexpression.line(), "Function call has too many actual parameters.");
            
            return new TypeClass(IntegerType.instance(), null);
        }
        
        TypeClass argtc;
        Type argType;
        Vector actuals = new Vector();
        for (int i=0; i<callexpression.size(); i++) {
            //check to see if the args used have the types they are supposed to have
            argtc = (TypeClass) callexpression.elementAt(i).Accept(this);
            argType = argtc.type();
            actuals.insertElementAt(argtc.value(), i);
            if (funcEntry.formals().elementAt(i) != argType) {
                CompError.message(callexpression.line(), "Argument " + i + " for function " + callexpression.name() + " does not match"
                        + " its corresponding function parameter's type.");
                return new TypeClass(IntegerType.instance(), null);
            }
        }
        return new TypeClass(funcEntry.result(), bt.callExpression(actuals, functionEnv.find(callexpression.name()).startlabel()));
    }   /* DONE */
    
    public Object VisitFunctionCallStatement(ASTFunctionCallStatement statement) {
        ////System.out.println("VisitFunctionCallStatement() LINE: " + statement.line());
        //check to see if function exists in func env.
        FunctionEntry funcEntry = functionEnv.find(statement.name());
        if (funcEntry == null) {
            CompError.message(statement.line(), "Function " + statement.name() + " is not defined in this " +
                              "scope");
        }
        if (statement.size() < funcEntry.formals().size()) {
            CompError.message(statement.line(), "Function call has too few actual parameters.");

            return null;
        }
        if (statement.size() > funcEntry.formals().size()) {
            CompError.message(statement.line(), "Function call has too many actual parameters.");
            return null;
        }
        //Check Return Type - if not void, don't allow
        if (funcEntry.result() != VoidType.instance()) {
            CompError.message(statement.line(), statement.name() + " is not a void function.");
            return null;
        }
        TypeClass argtc;
        Type argType;
        Vector actuals = new Vector();        
        for (int i=0; i<statement.size(); i++) {           
            argtc = (TypeClass) statement.elementAt(i).Accept(this);
            argType = argtc.type();
            actuals.insertElementAt(argtc.value(), i);
            //check to see if the args used have the types they are supposed to have
            if (funcEntry.formals().elementAt(i) != argType) {
                CompError.message(statement.line(), "Argument " + i + " for function " + statement.name() + " does not match"
                        + " its corresponding function parameter's type.");
                return null;
            }
        }
        return bt.callStatement(actuals, functionEnv.find(statement.name()).startlabel());
    }   /* DONE */
    
    public Object VisitInstanceVariableDefs(ASTInstanceVariableDefs variabledefs) {
        //////System.out.println("VisitInstanceVariableDefs()");
        for (int i=0; i<variabledefs.size(); i++) {
            //Check if type exists by calling VisitInstanceVariableDef() which calls CheckType(), which
            //checks the type and adds the according n-dimensional array types if the base type exists.
            //No need to add variable to environment because it is added into a local environment in VisitClass()
            variabledefs.elementAt(i).Accept(this);
        }
        return null;
    }   /* DONE */
    
    public Object VisitNewClassExpression(ASTNewClassExpression classexpression) {
        //check to see if the type is valid, in this case the type is a custom class type
        ClassType classType = (ClassType) typeEnv.find(classexpression.type());
        if (classType == null) {
            CompError.message(classexpression.line(), "Class type" + classexpression.type() + " is not defined in this " +
                    "scope");
            return new TypeClass(IntegerType.instance(), null);
        }
        int size = classType.variables().size() * MachineDependent.WORDSIZE;
        return new TypeClass(classType, bt.allocate(bt.constantExpression(size)));
    }   /* DONE */
    
    public Object VisitOperatorExpression(ASTOperatorExpression opexpression) {
        TypeClass left = (TypeClass) opexpression.left().Accept(this);
        TypeClass right = (TypeClass) opexpression.right().Accept(this);
        AATExpression leftv = left.value();
        AATExpression rightv = right.value();
        Type lhs = left.type();
        Type rhs = right.type(); 
    
        // NOTE: "Not" operator is taken care of by VisitUnaryOperatorExpression()
        
        switch (opexpression.operator()) {
            case ASTOperatorExpression.BAD_OPERATOR:    // Is this necessary?
                return new TypeClass(IntegerType.instance(), null);
                
            case ASTOperatorExpression.PLUS:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "+ operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(IntegerType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.PLUS));
            case ASTOperatorExpression.MINUS:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "- operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(IntegerType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.MINUS));
            case ASTOperatorExpression.MULTIPLY:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "* operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(IntegerType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.MULTIPLY));
            case ASTOperatorExpression.DIVIDE:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "/ operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(IntegerType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.DIVIDE));

                
            case ASTOperatorExpression.GREATER_THAN:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "> (greater_than) operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.GREATER_THAN));
            case ASTOperatorExpression.GREATER_THAN_EQUAL:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), ">= (greater_than_equal) operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.GREATER_THAN_EQUAL));
            case ASTOperatorExpression.LESS_THAN:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "< (less_than) operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.LESS_THAN));
            case ASTOperatorExpression.LESS_THAN_EQUAL:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "<= (less_than_equal) operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.LESS_THAN_EQUAL));
            case ASTOperatorExpression.EQUAL:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "= operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.EQUAL));
            case ASTOperatorExpression.NOT_EQUAL:
                if (lhs != IntegerType.instance() || rhs != IntegerType.instance()) {
                    CompError.message(opexpression.line(), "!= operator requires integer operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.NOT_EQUAL));

                
            case ASTOperatorExpression.AND:
                if (lhs != BooleanType.instance() || rhs != BooleanType.instance()) {
                    CompError.message(opexpression.line(), "AND operator requires boolean operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.AND));
            case ASTOperatorExpression.OR:
                if (lhs != BooleanType.instance() || rhs != BooleanType.instance()) {
                    CompError.message(opexpression.line(), "OR operator requires boolean operands");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(leftv, rightv, AATOperator.OR));
        }
                
        return IntegerType.instance();
    }   /* DONE */
    
    public Object VisitFunctionDefinitions(ASTFunctionDefinitions fundefinitions) {
        AATSequential tree = null;
        AATSequential topoftree = null;
        AATStatement fun = null;
        boolean startedtree = false;        
        for (int i=0; i < fundefinitions.size(); i++) {
            fun = (AATStatement) fundefinitions.elementAt(i).Accept(this);
            if (fun == null) {
                continue;
            } else if (!startedtree && i == fundefinitions.size()-1) {    //only one statement
                variableEnv.endScope();
                return fun;
            } else if (!startedtree && i != fundefinitions.size()-1){
                tree = (AATSequential) bt.sequentialStatement(fun, null);
                topoftree = tree;
                startedtree = true;
            } else if (i == fundefinitions.size()-1) {
                tree.setright(fun);
            } else {
                tree.setright((AATSequential) bt.sequentialStatement(fun, null));
                tree = (AATSequential) tree.right();
            }
        }
        return topoftree;
    }   /* DONE */
    
    public Object VisitReturnStatement(ASTReturnStatement returnstatement) {
        ////System.out.println("VisitReturnStatement() LINE: " + returnstatement.line());
        //Compare return type of specific function in environment and type of the expression being returned.
        //Error if they don't match.
        //retrieve the type of the function by looking up return in the variable environment
        VariableEntry returnEntry = variableEnv.find("return");
        //compare this type with the typeof returnstatement
        TypeClass returntypetc = null;
        Type returntype;
        if (returnstatement.value() != null) {
            returntypetc = (TypeClass) returnstatement.value().Accept(this);
            returntype = returntypetc.type();
        } else {
            returntype = VoidType.instance();
        }
        ////System.out.println("RETURN: "+returntype + " " + returnEntry.type());
        if (returntype != returnEntry.type()) {
            CompError.message(returnstatement.line(), "Return statement type "
                              + "does not match with the type given to the function.");
        }
        FunctionEntry funcEntry = functionEnv.find(currentFunctionName);
        if (returntype == VoidType.instance()) {
            return bt.returnStatement(bt.constantExpression(0), funcEntry.endlabel());
        } else {
            return bt.returnStatement(returntypetc.value(), funcEntry.endlabel());
        }
    }   /* DONE */
    
    /**
     * Adds a description of this function to the function environment
     * - Type of each parameter
     * - Return type of the function
     * 
     * @param prototype
     */
    public Object VisitPrototype(ASTPrototype prototype) {
        ResetOffsets(); //Should reset because if there is prototype, function will not re-add formals to environ. Therefore, offsets must be correct)
        //////System.out.println("VisitPrototype()");
        //Add prototype to function environment
        Vector<Type> params = new Vector<Type>();
        if (prototype.formals() != null) {
            Type paramType;
            for (int i=0; i < prototype.formals().size(); i++) {
                paramType = (Type) prototype.formals().elementAt(i).Accept(this);
                params.add(paramType);
            }
        }
        Type type = ReturnTypeHelper(prototype.type(), prototype.line());
        functionEnv.insert(prototype.name(), new FunctionEntry(type, params, new Label(prototype.name()), new Label(prototype.name())));
        return null;
    }   /* DONE */
    
    /**
     * Checks if return type is an array. Calls CheckType.
     * 
     * @param type
     * @param linenum
     * @return
     */
    public Type ReturnTypeHelper(String type, int linenum) {
        //////System.out.println("ReturnTypeHeler()");
        int dimensionality = 0;
        int i;
        for (i = type.length() - 1; i >= 0; i--) {
            if (type.charAt(i) == ']') {
                dimensionality ++;
            } else if (type.charAt(i) != '[') {
                break;
            }
        }
        type = type.substring(0, i+1);    //Base Type
        //////System.out.println("Base Type: " + type + " Dimensionality: " + dimensionality);
        return CheckType(type, dimensionality, linenum);
    }   /* DONE */
    
    public Object VisitUnaryOperatorExpression(ASTUnaryOperatorExpression unaryexpression) {
        //Deal with logical "NOT"
        
        TypeClass operandtc = (TypeClass) unaryexpression.operand().Accept(this);
        Type operand = operandtc.type();
        
        switch (unaryexpression.operator()) {
            case ASTUnaryOperatorExpression.BAD_OPERATOR:    // Is this necessary?
                return new TypeClass(IntegerType.instance(), null);
                
            case ASTUnaryOperatorExpression.NOT:
                if (operand != BooleanType.instance()) {
                    CompError.message(unaryexpression.line(), "NOT operators requires "
                            + "a boolean operand.");
                    return new TypeClass(IntegerType.instance(), null);
                }
                return new TypeClass(BooleanType.instance(), bt.operatorExpression(operandtc.value(), null, AATOperator.NOT));  //TODO: are we correct in using operator expression?
        }
        return new TypeClass(IntegerType.instance(), null);
    }   /* DONE */
    
    public Object VisitStatements(ASTStatements statements) {
        AATSequential tree = null;
        AATSequential topoftree = null;
        AATStatement sm = null;
        boolean startedtree = false;
        variableEnv.beginScope();
        for (int i = 0; i<statements.size(); i++) {
            sm = (AATStatement) statements.elementAt(i).Accept(this);
            if (sm == null) {
                continue;
            }
            else if (!startedtree && i == statements.size()-1){    //only one statement
                AATStatement singletree = sm;
                variableEnv.endScope();
                return singletree;
            }            
            else if (!startedtree && i != statements.size()-1){
                tree = (AATSequential) bt.sequentialStatement(sm, null);
                topoftree = tree;
                startedtree = true;
            } else if (i == statements.size()-1) {
                tree.setright(sm);
            } else {
                tree.setright((AATSequential) bt.sequentialStatement(sm, null));
                tree = (AATSequential) tree.right();
            }
        }
        variableEnv.endScope();
        return topoftree;
    }   /* DONE */
    
    public Object VisitVariableExpression(ASTVariableExpression varexpression) {
        TypeClass variableexpression = (TypeClass) varexpression.variable().Accept(this);
        return variableexpression;
    }
    
    public Object VisitVariableDefStatement(ASTVariableDefStatement varstatement) {
        int varoffset = offset;
        
        //Checks Type
        Type type = CheckType(varstatement.type(), varstatement.arraydimension(), varstatement.line());
        TypeClass inittc = null;
        
        //Compare lhs and rhs
        if (varstatement.init() != null) {
            inittc = (TypeClass) varstatement.init().Accept(this);
            Type inittype = inittc.type();
            if (type != inittype) {
                CompError.message(varstatement.line(), "Types did not match in variable definition statement: " + 
                        varstatement.name() + ". ");
            }
        }
        
        //Check variable name
        if (variableEnv.find(varstatement.name()) != null) {
            CompError.message(varstatement.line(), "Duplicate local variable " + 
                    varstatement.name() + ". ");
        } else {
            variableEnv.insert(varstatement.name(), new VariableEntry(type, IncrementOffset()));
        }
        
        if (varstatement.init() != null) {
            AATMemory lhs = new AATMemory(bt.operatorExpression(new AATRegister(Register.FP()), bt.constantExpression(varoffset), AATOperator.MINUS));
            return bt.assignmentStatement(lhs, inittc.value());
        } else {
            return null;
        }
    }   /* DONE */

    public Object VisitProgram(ASTProgram program) {
        program.classes().Accept(this);
        return program.functiondefinitions().Accept(this);
    }   /* DONE */
    
    public Object VisitWhileStatement(ASTWhileStatement whilestatement) {
        //////System.out.println("While (test/body)");
        TypeClass test = (TypeClass) whilestatement.test().Accept(this);

        if (test.type() != BooleanType.instance()) {
            CompError.message(whilestatement.line(), "While test must be a boolean");
        }

        AATStatement body = null;
        if (whilestatement.body() != null) {
            variableEnv.beginScope();
            body = (AATStatement) whilestatement.body().Accept(this);
            variableEnv.endScope();
        }
        return bt.whileStatement(test.value(), body);
    }   /* DONE */

    public Object VisitIntegerLiteral(ASTIntegerLiteral literal) {
        return new TypeClass(IntegerType.instance(), bt.constantExpression(literal.value()));
    }   /* DONE */

    public Object VisitBaseVariable(ASTBaseVariable base) {
        //System.out.println("VisitBaseVariable() LINE: "+base.line() + " BASE: "+ base.name());
        VariableEntry baseEntry = variableEnv.find(base.name());
        if (baseEntry == null) {
            CompError.message(base.line(), "Variable " + base.name() + " is not defined in this " +
                    "scope");
            //return IntegerType.instance();
            return new TypeClass(IntegerType.instance(), null);
        } else {
            return new TypeClass(baseEntry.type(), bt.baseVariable(baseEntry.offset()));
            //return baseEntry.type();

        }
    }   /* DONE */

    public Object VisitIfStatement(ASTIfStatement ifsmt) {

        TypeClass test = (TypeClass) ifsmt.test().Accept(this);

        if (test.type() != BooleanType.instance()) {
            CompError.message(ifsmt.line(), "If test must be a boolean");
        }

        variableEnv.beginScope();
        AATStatement thenstmt = (AATStatement) ifsmt.thenstatement().Accept(this);
        variableEnv.endScope();

        AATStatement elsestmt = null;
        if(ifsmt.elsestatement() != null) {
            variableEnv.beginScope();
            elsestmt = (AATStatement) ifsmt.elsestatement().Accept(this);
            variableEnv.endScope();
        }

        return bt.ifStatement(test.value(), thenstmt, elsestmt);
    }   /* DONE */
    
    /**
     * Returns current offset and then increments the offset.
     * @return current offset
     */
    public int IncrementOffset() {
        int oldOffset = offset;
        offset += MachineDependent.WORDSIZE;
        return oldOffset;
    }   /* DONE */
    
    /**
     * Returns current formal offset and then decrements the formal offset.
     * @return current formal offset
     */
    public int DecrementFormalOffset() {
        int oldOffset = formalOffset;
        formalOffset -= MachineDependent.WORDSIZE;
        return oldOffset;
    }   /* DONE */
    
    public void ResetOffsets() {
        formalOffset = (-1 * MachineDependent.WORDSIZE);
        offset = 0;
    }   /* DONE */
}
