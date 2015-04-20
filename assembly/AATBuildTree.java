import java.util.Vector;

public class AATBuildTree {
  
    public AATStatement functionDefinition(AATStatement body, int framesize, Label start, Label end) {
        AATRegister fp = new AATRegister(Register.FP());
        AATRegister sp = new AATRegister(Register.SP());
        AATRegister ra = new AATRegister(Register.ReturnAddr());
        AATExpression wordsize = constantExpression(MachineDependent.WORDSIZE);
        AATExpression framesizetree = constantExpression(framesize);
        AATLabel startlabeltree = new AATLabel(start);
        AATLabel endlabeltree = new AATLabel(end);
        AATStatement tree;
        /** Set up Activation Record */
        /* 1) put sp, fp, and return addr onto stack. Decrement stack. */
        //save FP to -(framesize)($sp)
        AATMove savefp = new AATMove(                            
                                new AATMemory(
                                operatorExpression(
                                        sp, 
                                        framesizetree,
                                        AATOperator.MINUS
                                )
                            ),
                            fp
                        );
        //save SP to -(framesize + 4)($sp)
        AATMove savesp = new AATMove(
                            new AATMemory(
                                    operatorExpression(
                                            sp, 
                                            operatorExpression(
                                                    framesizetree,
                                                    wordsize,
                                                    AATOperator.PLUS),
                                            AATOperator.MINUS
                                    )
                            ),
                            sp
                         );
        //save RA to -(framesize + 8)($sp)
        AATMove savera = new AATMove(
                            new AATMemory(
                                    operatorExpression(
                                            sp,
                                            operatorExpression( //framesize + 8
                                                    framesizetree,
                                                    operatorExpression(     //2 * WORDSIZE = 8
                                                            wordsize,
                                                            constantExpression(2), 
                                                            AATOperator.MULTIPLY
                                                    ),
                                                    AATOperator.PLUS
                                            ),
                                            AATOperator.MINUS
                                    )
                            ),
                            ra
                         );  
                                             
        /* 2) set fp to sp */  
        AATMove setfp = new AATMove(fp, sp);
        
        /* 3) decrement sp to accommodate the space for local variables and saved registers */
        //sp = sp - framesize - saved registers
        AATMove decrementsp = new AATMove(
                                        sp,
                                        operatorExpression(
                                                sp,
                                                operatorExpression(     //framesize + 12
                                                        framesizetree,
                                                        operatorExpression(
                                                                wordsize,
                                                                constantExpression(3),
                                                                AATOperator.MULTIPLY
                                                        ),
                                                        AATOperator.PLUS
                                                ),
                                                AATOperator.MINUS
                                        )
                                    );
        
        /** Clean up activation record */
        /* 1) Restore Return Address: -(framesize + 8)($fp) */
        AATMove restorera = new AATMove(
                ra,
                new AATMemory(
                        operatorExpression(
                                fp,
                                operatorExpression( //framesize + 8
                                        framesizetree,
                                        operatorExpression(     //2 * WORDSIZE = 8
                                                wordsize,
                                                constantExpression(2), 
                                                AATOperator.MULTIPLY
                                        ),
                                        AATOperator.PLUS
                                ),
                                AATOperator.MINUS
                        )
                )
             ); 
        /* 2) Restore Stack Pointer: -(framesize + 4)($fp) */
        AATMove restoresp = new AATMove(
                sp,
                new AATMemory(
                        operatorExpression(
                                fp, 
                                operatorExpression(
                                        framesizetree,
                                        wordsize,
                                        AATOperator.PLUS),
                                AATOperator.MINUS
                        )
                )
             ); 
        /* 3) Restore Frame Pointer: -(framesize)($fp) */
        AATMove restorefp = new AATMove(
                fp,
                new AATMemory(
                    operatorExpression(
                            fp, 
                            framesizetree,
                            AATOperator.MINUS
                    )
                )
             ); 
        tree = sequentialStatement(restorefp, new AATReturn());
        tree = sequentialStatement(restoresp, tree);
        tree = sequentialStatement(restorera, tree);
        tree = sequentialStatement(endlabeltree, tree);         /* End label */
        if (body != null)
            tree = sequentialStatement(body, tree);                 /* Add body */
        tree = sequentialStatement(decrementsp, tree);
        tree = sequentialStatement(setfp, tree);
        tree = sequentialStatement(savera, tree);
        tree = sequentialStatement(savesp, tree);
        tree = sequentialStatement(savefp, tree);
        tree = sequentialStatement(startlabeltree, tree);       /* Start Label */
        return tree;
    }   /* DONE */
    
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
        Label allocatelabel = Label.AbsLabel("allocate");
        Vector sizevector = new Vector();
        sizevector.add(size);
        return callExpression(sizevector, allocatelabel);
    }   /* DONE */

    public AATStatement whileStatement(AATExpression test, AATStatement whilebody) {
	Label whiletestlabel = new Label("whiletest");
	Label whilestartlabel = new Label("whilestart");
	AATLabel whiletesttree = new AATLabel(whiletestlabel);
	AATLabel whilestarttree = new AATLabel(whilestartlabel);
	
	AATStatement tree = sequentialStatement(whiletesttree, new AATConditionalJump(test, whilestartlabel));
	if (whilebody!= null) {
	    tree = sequentialStatement(whilebody, tree);
	}
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


