/*
 * Copyright (c) 2004, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

/*
 * @test
 * @summary Basic functional test of remove method for InheritableThreadLocal
 * @author Seetharama Avadhanam
 */
import java.util.Arrays;

public class ITLRemoveTest {
    private static final int INITIAL_VALUE = Integer.MIN_VALUE;
    private static final int REMOVE_SET_VALUE = Integer.MAX_VALUE;

    static InheritableThreadLocal<Integer> n = new InheritableThreadLocal<Integer>() {
        protected Integer initialValue() {
            return INITIAL_VALUE;
        }

        protected Integer childValue(Integer parentValue) {
            return(parentValue + 1);
        }
    };

    static final int THREAD_COUNT = 100;
    // final array but its items will be modified
    static final int x[]= new int[THREAD_COUNT];
    // final array but its items will be modified
    static final Throwable exceptions[]= new Throwable[THREAD_COUNT];
    
    static final int[] removeNode = {20,38,45,88};
    /* ThreadLocal values will be removed for these threads. */
    static final int[] removeAndSet = {10, 12,34};
    /* ThreadLocal values will be removed and sets new values */

    public static void main(String args[]) throws Throwable {
        final Thread progenitor = new MyThread();
        progenitor.start();

        // Wait for *all* threads to complete
        progenitor.join();

        for(int i = 0; i<THREAD_COUNT; i++){
            final int checkValue;

            /* If the remove method is called then the ThreadLocal value will
             * be its initial value */
            if( Arrays.binarySearch( removeNode, i)>=0 ) {
                checkValue = INITIAL_VALUE; // NOT based on parent thread's value!
            }
            else
            if( Arrays.binarySearch( removeAndSet, i)>=0 ) {
                checkValue = REMOVE_SET_VALUE;
            }
            else {
                checkValue = i+INITIAL_VALUE;
            }

            if(exceptions[i] != null)
                throw(exceptions[i]);
            if(x[i] != checkValue)
                throw(new Throwable("x[" + i + "] =" + x[i]));
        }
    }
    private static class MyThread extends Thread {
        public void run() {
            Thread child = null;
            final int threadId= n.get();
            try{
                // Creating child thread...
                if (threadId < (THREAD_COUNT-1+INITIAL_VALUE)) {
                    child = new MyThread();
                    child.start();
                }
                // threadLocal.get() must work even if the parent thread finishes, and it shouldn't lock the parent thread. Plus, yield() in the following is a hint only. Hence the test should work without it, too.
                
                /*for (int j = 0; j<threadId; j++)
                    Thread.currentThread().yield();/**/

                final int threadPosition= threadId-INITIAL_VALUE; // 0-based position within all non-main threads
                // To remove the ThreadLocal value...
                if( Arrays.binarySearch(removeNode, threadPosition)>=0 ) {
                    n.remove();
                }
                /*for(int removeId  : removeNode)
                   if((threadId-INITIAL_VALUE) == removeId){
                       n.remove();
                       break;
                   }*/

                 // To remove the ThreadLocal value and set new value ...
                 else
                 if( Arrays.binarySearch(removeAndSet, threadPosition)>=0 ) {
                        n.remove();
                        n.set(REMOVE_SET_VALUE);
                 }
                 /*for(int removeId  : removeAndSet)
                    if((threadId-INITIAL_VALUE) == removeId){
                        n.remove();
                        n.set(REMOVE_SET_VALUE);
                        break;
                    }*/
                x[threadId-INITIAL_VALUE] =  n.get();
            }catch(Throwable ex){
                exceptions[threadId-INITIAL_VALUE] = ex;
            }
             // Wait for child (if any)
            if (child != null) {
                try {
                     child.join();
                } catch(InterruptedException e) {
                     throw(new RuntimeException("Interrupted"));
                }
            }
        }
    }
}
