import java.util.Vector;

public class AATBuildTree {
  
    public AATStatement functionDefinition(AATStatement body, int framesize, Label start,  
					   Label end) {
        AATExpression framepointer;
        AATExpression stackpointer;
        AATExpression returnvalue;
        AATLabel startlabeltree = new AATLabel(start);
        AATLabel endlabeltree = new AATLabel(end);
        //copy value to Result register
        //move values from fp register to memory
        AATMove saveframemove = new AATMove(new AATOperator(new AATRegister(Register.FP()), new AATConstant(0 * WORDSIZE), AATOperator.MINUS), framepointer);
        //move from sp register to memory
        AATMove savestackmove = new AATMove(new AATOperator(new AATRegister(Register.FP()), new AATConstant(1 * WORDSIZE), AATOperator.MINUS), stackpointer)
        //move return register to memory
        new AATMove(new AATOperator(new AATRegister(Register.FP()), new AATConstant(2 * WORDSIZE), AATOperator.MINUS), returnvalue)
        AATStatement functionDefinitiontree = sequentialStatement(saveframemove,startlabeltree);
        functionDefinitiontree = sequentialStatement(savestackmove,functionDefinitiontree);
        functionDefinitiontree = sequentialStatement(savereturnvalue, functionDefinitiontree);
        //have the fp point to the beginning of the stack
        //fp is equal to sp
        AATMove fppointtobegofstackframe = new AATMove(stackpointer,new AATRegister(Register.FP()));
        functionDefinitiontree = sequentialStatement(fppointtobegofstackframe, functionDefinitiontree);
        //decrement sp here by subtracting framesize from fp
        AATMove sppointertoendofframe = new AATMove((new AATOperator(new AATRegister(Register.FP()), new AATConstant(framesize), AATOperator.MINUS)),new AATRegister(Register.SP()));
        functionDefinitiontree = sequentialStatement(sppointertoendofframe, functionDefinitiontree);
        functionDefinitiontree = sequentialStatement(endlabeltree,functionDefinitiontree);
        //restore old values of the Stack Pointer, Frame Pointer, and Return Address registers here
        AATMove restorestackpointer = new AATMove(stackpointer, new AATRegister(Register.SP());
        AATMove restorereturnaddress = new AATMove(returnvalue, new AATRegister(Register.ReturnAddr()));
        AATMove restoreframepointer = new AATMove(framepointer, new AATRegister(Register.FP());
        functionDefinitiontree = sequentialStatement(restorestackpointer, functionDefinitiontree);
        functionDefinitiontree = sequentialStatement(restorereturnaddress, functionDefinitiontree);
        functionDefinitiontree = sequentialStatement(restoreframepointer, functionDefinitiontree);
        return fun;
    }
    
    public AATStatement ifStatement(AATExpression test, AATStatement ifbody, AATStatement elsebody) {
        Label ifendlabel = new Label("ifend");
        Label iftruelabel = new Label("iftrue");
        AATLabel ifendtree = new AATLabel(ifendlabel);
        AATLabel iftruetree = new AATLabel(iftruelabel);
        
        AATStatement tree = sequentialStatement(ifbody, ifendtree);
        tree = sequentialStatement(iftruetree, tree);
        tree = sequentialStatement(new AATJump(ifendlabel), tree);
        if (elsebody != null) {
            tree = sequentialStatement(elsebody, tree);
        }        
        return sequentialStatement(new AATConditionalJump(test, iftruelabel), tree);
    }   /* DONE */
    
    /**
     * Allocate creates an AAT that makes a call to the built-in function allocate, which takes as 
     * input the size (in bytes) to allocate, and returns a pointer to the beginning of the allocated
     * block.
     * 
     * @param size
     * @return
     */
    public AATExpression allocate(AATExpression size) {
        Label allocatelabel = AbsLabel("allocate");
        Vector sizevector = new Vector();
        sizevector.add(size);
        return callExpression(sizevector, allocatelabel);
        
    }

    public AATStatement whileStatement(AATExpression test, AATStatement whilebody) {
	Label whiletestlabel = new Label("whiletest");
	Label whilestartlabel = new Label("whilestart");
	AATLabel whiletesttree = new AATLabel(whiletestlabel);
	AATLabel whilestarttree = new AATLabel(whilestartlabel);
	
	AATStatement tree = sequentialStatement(whiletesttree, new AATConditionalJump(test, whilestartlabel));
	tree = sequentialStatement(whilebody, tree);
	tree = sequentialStatement(whilestarttree, tree);
	return sequentialStatement(new AATJump(whiletestlabel), tree);
    }   /* DONE */

    /**
     * doWhileStart:
     *          dowhilebody
     * doWhileTest:
     *          if (test) goto doWhileStart
     *          
     * @param test
     * @param dowhilebody
     * @return
     */
    public AATStatement dowhileStatement(AATExpression test, AATStatement dowhilebody) {
        Label dowhiletestlabel = new Label("dowhiletest");
        Label dowhilestartlabel = new Label("dowhilestart");
        AATLabel dowhiletesttree = new AATLabel(dowhiletestlabel);
        AATLabel dowhilestarttree = new AATLabel(dowhilestartlabel);
        
        AATStatement tree = sequentialStatement(dowhiletesttree, new AATConditionalJump(test, dowhilestartlabel));
        tree = sequentialStatement(dowhilebody, tree);
        return sequentialStatement(dowhilestarttree, tree);
    }   /* DONE */
  
    /**
     *          initialize
     *          goto FORTEST
     * FORSTART:
     *          statement
     *          increment
     * FORTEST:
     *          if (test) goto FORSTART 
     * 
     * @param init
     * @param test
     * @param increment
     * @param body
     * @return
     */
    public AATStatement forStatement(AATStatement init, AATExpression test, 
				     AATStatement increment, AATStatement body) {
        Label fortestlabel = new Label("fortest");
        Label forstartlabel = new Label("forstart");
        AATLabel fortesttree = new AATLabel(fortestlabel);
        AATLabel forstarttree = new AATLabel(forstartlabel);
        AATStatement tree = sequentialStatement(fortesttree, new AATConditionalJump(test, forstartlabel));
        tree = sequentialStatement(increment, tree);
        tree = sequentialStatement(body, tree);
        tree = sequentialStatement(forstarttree, tree);
        tree = sequentialStatement(new AATJump(fortestlabel), tree);
        return sequentialStatement(init, tree);
    }   /* DONE */
    
    public AATStatement emptyStatement() {
	return new AATEmpty();
    }   /* DONE */
  
    public AATStatement callStatement(Vector actuals, Label name) {
	return new AATCallStatement(name, actuals);
    }   /* DONE */
    
    public AATStatement assignmentStatement(AATExpression lhs,
					    AATExpression rhs) {
	return new AATMove(lhs, rhs);
    }   /* DONE */
    
    public AATStatement sequentialStatement(AATStatement first,
					    AATStatement second) {
	return new AATSequential(first, second);

    }   /* DONE */
    
    public AATExpression baseVariable(int offset) {
	return new AATMemory(new AATOperator(new AATRegister(Register.FP()),
					     new AATConstant(offset),
					     AATOperator.MINUS)) ;
    }   /* DONE */

    public AATExpression arrayVariable(AATExpression base,
				       AATExpression index,
				       int elementSize) {
	AATExpression offset = new AATOperator(constantExpression(elementSize), index, AATOperator.MULTIPLY);
	return new AATMemory(new AATOperator(base, offset, AATOperator.MINUS));
    }   /* DONE */
    
    public AATExpression classVariable(AATExpression base, int offset) {
	return new AATMemory(new AATOperator(base, 
	                                    constantExpression(offset),        //offset is known at compile time
	                                    AATOperator.MINUS));
    }   /* DONE */
  
    public AATExpression constantExpression(int value) {
	return new AATConstant(value);
    }   /* DONE */ 
  
    public AATExpression operatorExpression(AATExpression left,
					    AATExpression right,
					    int operator) {
	return new AATOperator(left, right, operator);
    }   /* DONE */
  
    public AATExpression callExpression(Vector actuals, Label name) {
	return new AATCallExpression(name, actuals);
    }   /* DONE */
    
    public AATStatement returnStatement(AATExpression value, Label functionend) {
        //copy value to Result register
        AATMove move = new AATMove(new AATRegister(Register.Result()), value);
        //jump to label
        AATJump jump = new AATJump(functionend);
        return sequentialStatement(move, jump);
    }   /* DONE */
}


