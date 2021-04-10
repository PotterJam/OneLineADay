export { isLoggedIn, signup, login }
export type { signupForm, loginForm }

import { getData, postData } from './Api';

type signupForm = {
    signupUsername: string,
    signupEmail: string,
    signupPassword: string
};

type loginForm = {
    loginUsername: string,
    loginPassword: string
};

const isLoggedIn = async ():Promise<boolean> => {
    const resp = await getData('/api/isLoggedIn');
    return resp.ok;
};

const signup = async (signupForm): Promise<boolean> => {
    const resp = await postData('/api/signup', signupForm);
    return resp.ok;
}
  
const login = async (loginForm): Promise<boolean> => {
    const resp = await postData('/api/login', loginForm);
    return resp.ok;
}