package com.se.idms.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashSet;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.executable.ExecutableValidator;

/**
 * Proxy Invocation Handler which uses {@link Validator} to validate the parameters and the return value before and
 * after the actual method invocation.
 */
public class ValidatingInvocationHandler<T> implements InvocationHandler {
    
    /**
     * The validator instance.
     */
    private static Validator validator = Validation.buildDefaultValidatorFactory().getValidator();
    
    /**
     * Instance to which calls are delegated before / after validation is performed.
     */
    private T delegate;
    
    /**
     * Construct an instance of the handler that delegates invocations to the supplied delegate.
     *
     * @param delegate instance to which calls are delegated before / after validation is performed
     */
    public ValidatingInvocationHandler(T delegate) {
    	this.delegate = delegate;
    }
    
    /**
     * Create a validating service proxy using the {@link ValidatingInvocationHandler}.
     *
     * @param delegateImpl instance to which calls are delegated before / after validation is performed
     * @param intf         interface class for the delegate exposed by the proxy
     * @param <I>          interface class for the delegate exposed by the proxy
     * @return validating proxy instance
     */
    @SuppressWarnings("unchecked")
    public static <I> I createValidatingServiceProxy(I delegateImpl, Class<I> intf) {
        return (I) Proxy.newProxyInstance(
                delegateImpl.getClass().getClassLoader(),
                new Class[]{intf},
                new ValidatingInvocationHandler<>(delegateImpl));
    }
    
    
    /**
     * Validates parameters before method invocation. If there are no violations, it proceeds to invoke the method
     * via the delegate. Finally, the return value from this invocation is again validated.
     *
     * @param proxy  proxy instance that the method was invoked on
     * @param method method to be invoked
     * @param args   arguments to be passed to the method on invocation
     * @return return value from the method invocation
     * @throws Throwable {@link ConstraintViolationException} containing the validation violations (if any) in either
     *                   the parameters or the return value; OR any other exception during the method invocation
     */
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        Object retValue = null;
        
        ExecutableValidator methodValidator = validator.forExecutables();
        
        Set<? extends ConstraintViolation<?>> violations = new HashSet<>();
        if (args != null && args.length != 0) {
            violations = methodValidator.validateParameters(delegate, method, args);
        }
        if (violations.size() == 0) {
            try {
                // TODO: Handle void return type
                retValue = method.invoke(delegate, args);
            } catch (InvocationTargetException e) {
                throw e.getCause();
            }
            violations = methodValidator.validateReturnValue(delegate, method, retValue);
        }
        
        if (violations.size() > 0) {
            throw new ConstraintViolationException(violations);
        } else {
            return retValue;
        }
    }
}
