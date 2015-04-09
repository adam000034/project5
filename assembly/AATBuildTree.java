import java.util.Vector;

public class AATBuildTree {
  
    public AATStatement functionDefinition(AATStatement body, int framesize, Label start,  
					   Label end) {
	
	return null;
    }
    
    public AATStatement ifStatement(AATExpression test, AATStatement ifbody, AATStatement elsebody) {
	return null;
    }
    
    /**
     * Allocate creates an AAT that makes a call to the built-in function allocate, which takes as 
     * input the size (in bytes) to allocate, and returns a pointer to the beginning of the allocated
     * block.
     * 
     * @param size
     * @return
     */
    public AATExpression allocate(AATExpression size) {
	return null;
    }

    public AATStatement whileStatement(AATExpression test, AATStatement whilebody) {
	return null;
    }

    public AATStatement dowhileStatement(AATExpression test, AATStatement dowhilebody) {
	return null;
    }
  
    public AATStatement forStatement(AATStatement init, AATExpression test, 
				     AATStatement increment, AATStatement body) {
	return null;
    }
    
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
	return null;
    }
    
    public AATExpression classVariable(AATExpression base, int offset) {
	return null;
    }
  
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
        //TODO: Now what? Is it Sequential? or...? Or would we use AATReturn?
        return sequentialStatement(move, jump);
    }
}


