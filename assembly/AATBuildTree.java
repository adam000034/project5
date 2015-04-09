import java.util.Vector;

public class AATBuildTree {
  
    public AATStatement functionDefinition(AATStatement body, int framesize, Label start,  
					   Label end) {
	
	return null;
    }
    
    public AATStatement ifStatement(AATExpression test, AATStatement ifbody, AATStatement elsebody) {
	return null;
    }
    
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
	return null;
    }
  
    public AATStatement callStatement(Vector actuals, Label name) {
	return null;
    }
    
    public AATStatement assignmentStatement(AATExpression lhs,
					    AATExpression rhs) {
	return null;
    }
    
    public AATStatement sequentialStatement(AATStatement first,
					    AATStatement second) {
	return null;
    }
    
    
    
    
    public AATExpression baseVariable(int offset) {
	return new AATMemory(new AATOperator(new AATRegister(Register.FP()),
					     new AATConstant(offset),
					     AATOperator.MINUS)) ;
    }






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
    }
  
    public AATExpression operatorExpression(AATExpression left,
					    AATExpression right,
					    int operator) {
	return null;
    }
  
    public AATExpression callExpression(Vector actuals, Label name) {
	return null;
    }
    
    public AATStatement returnStatement(AATExpression value, Label functionend) {
	return null;
    }
}


