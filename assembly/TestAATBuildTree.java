import java.util.Vector;

class TestAATBuildTree {

    public static void main(String args[]) {
	AATBuildTree bt = new AATBuildTree();
	AATPrintTree pt = new AATPrintTree();
	AATExpression exptree;
	AATStatement stmtree, functiondefs;
	Vector actuals;
    System.out.println("=================== ");
	System.out.println("Testing Expressions ");
	System.out.println("=================== ");
	System.out.println("----------------------------");
	System.out.println("Testing literal constat : 15 ");
	System.out.println("----------------------------");
	exptree =  bt.constantExpression(15);
	exptree.Accept(pt);
	System.out.println("--------------------------");
	System.out.println("Testing expression : 4 + 5 ");
	System.out.println("--------------------------");
	exptree = bt.operatorExpression(bt.constantExpression(4),bt.constantExpression(5),
					AATOperator.PLUS);
	exptree.Accept(pt);
	System.out.println("================= ");
	System.out.println("Testing Variables ");
	System.out.println("================= ");
	System.out.println("-------------------------------");
	System.out.println("Testing base variable, offset 4 ");
	System.out.println("-------------------------------");
	exptree = bt.baseVariable(4);
	exptree.Accept(pt);
	System.out.println("----------------------------------------");
	System.out.println("Assingment:  x := 3 (x local, offset = 4)");
	System.out.println("----------------------------------------");
	stmtree = bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3));
	stmtree.Accept(pt);
	System.out.println("-----------------------------------------------------------");
	System.out.println("For: for(i=0; i < 10; i++) x := 0;   x offset 4, i offset 8 ");
	System.out.println("------------------------------------------------------------");
	stmtree = bt.forStatement(bt.assignmentStatement(bt.baseVariable(8), bt.constantExpression(0)),
				  bt.operatorExpression(bt.baseVariable(8), bt.constantExpression(10), AATOperator.LESS_THAN),
				  bt.assignmentStatement(bt.baseVariable(8),
							 bt.operatorExpression(bt.baseVariable(8),
									       bt.constantExpression(1),
									       AATOperator.PLUS)),
				  bt.assignmentStatement(bt.baseVariable(4),
							 bt.constantExpression(0)));
        stmtree.Accept(pt);
        System.out.println("================= ");
        System.out.println("Testing Function Definition");
        stmtree = bt.functionDefinition(bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)), 200, new Label("start"), new Label("end"));
        stmtree.Accept(pt);
        System.out.println("================= ");
        System.out.println("-------------------------------");
        System.out.println("Testing base variable, offset 4 ");
        System.out.println("-------------------------------");
        exptree = bt.baseVariable(4);
        exptree.Accept(pt);
        System.out.println("----------------------------------------");
        System.out.println("Testing ifStatement");
        System.out.println("----------------------------------------");
        System.out.println("----------------------------------------");
        System.out.println("Testing allocate");
        System.out.println("----------------------------------------");
        stmtree = bt.ifStatement(bt.allocate(bt.constantExpression(100)),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(8)));
        
        stmtree.Accept(pt);
      
       
        System.out.println("----------------------------------------");
        System.out.println("Testing whilestatement");
        System.out.println("----------------------------------------");
        stmtree = bt.whileStatement(bt.operatorExpression(bt.baseVariable(8), bt.constantExpression(10), AATOperator.LESS_THAN),
                                    bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)));
        stmtree.Accept(pt);
        System.out.println("----------------------------------------");
        System.out.println("Testing dowhilestatement");
        System.out.println("----------------------------------------");
        stmtree = bt.dowhileStatement(bt.operatorExpression(bt.baseVariable(8), bt.constantExpression(10), AATOperator.LESS_THAN),
                                    bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)));
        stmtree.Accept(pt);
        
        System.out.println("----------------------------------------");
        System.out.println("Testing emptystatement");
        System.out.println("----------------------------------------");
        stmtree = bt.emptyStatement();
        stmtree.Accept(pt);
        
        System.out.println("----------------------------------------");
        
        
        System.out.println("Testing ifStatement");
        System.out.println("----------------------------------------");
        System.out.println("----------------------------------------");
        System.out.println("Testing arrayvariable");
        System.out.println("----------------------------------------");
        stmtree = bt.ifStatement(bt.arrayVariable(bt.baseVariable(4), bt.constantExpression(3), 4),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(8)));
        
        stmtree.Accept(pt);
        System.out.println("Testing ifStatement");
        System.out.println("----------------------------------------");
        System.out.println("----------------------------------------");
        System.out.println("Testing classvariable");
        System.out.println("----------------------------------------");
        stmtree = bt.ifStatement(bt.classVariable(bt.baseVariable(4), 4),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(8)));
        
        stmtree.Accept(pt);
        System.out.println("Testing ifStatement");
        System.out.println("----------------------------------------");
        System.out.println("----------------------------------------");
        System.out.println("Testing constantexpression");
        System.out.println("----------------------------------------");
        stmtree = bt.ifStatement(bt.constantExpression(4),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(8)));
        
        stmtree.Accept(pt);
        System.out.println("Testing ifStatement");
        System.out.println("----------------------------------------");
        System.out.println("----------------------------------------");
        System.out.println("Testing callExpression");
        System.out.println("----------------------------------------");
        Vector argscallexpress = new Vector();
        argscallexpress.add(new AATConstant(1));
        stmtree = bt.ifStatement(bt.callExpression(argscallexpress,new Label("test")),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(3)),
                                 bt.assignmentStatement(bt.baseVariable(4), bt.constantExpression(8)));
        
        stmtree.Accept(pt);
        System.out.println("----------------------------------------");
        System.out.println("Testing returnStatement");
        System.out.println("----------------------------------------");
        stmtree = bt.returnStatement(bt.constantExpression(3), new Label("testreturn"));
        stmtree.Accept(pt);
        System.out.println("-----------------------------------------------------------");
        System.out.println("For: for(i=0; i < 10; i++) x := 0;   x offset 4, i offset 8 ");
        System.out.println("------------------------------------------------------------");
        stmtree = bt.forStatement(bt.assignmentStatement(bt.baseVariable(8), bt.constantExpression(0)),
                                  bt.operatorExpression(bt.baseVariable(8), bt.constantExpression(10), AATOperator.LESS_THAN),
                                  bt.assignmentStatement(bt.baseVariable(8),
                                                         bt.operatorExpression(bt.baseVariable(8),
                                                                               bt.constantExpression(1),
                                                                               AATOperator.PLUS)),
                                  bt.assignmentStatement(bt.baseVariable(4),
                                                         bt.constantExpression(0)));
        stmtree.Accept(pt);
	/* Add more tests !!! */
        
    

    }
}





