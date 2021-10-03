export { updateLoginStatus, signup, login, logout }
export type { signupForm, loginForm }
export const loggedIn = writable(false);

import { getData, postData } from './Api';
import { writable } from 'svelte/store';
import { navigate } from "svelte-routing";

type signupForm = {
    signupUsername: string,
    signupEmail: string,
    signupPassword: string
};

type loginForm = {
    loginUsername: string,
    loginPassword: string
};

loggedIn.subscribe(authenticated => {
    if (authenticated) {
        navigate('/');
    }
});

const updateLoginStatus = async ():Promise<void> => {
    const resp = await getData('/api/isLoggedIn');
    loggedIn.update(_ => resp.ok);
};

const signup = async (signupForm): Promise<void> => {
    const resp = await postData('/api/signup', signupForm);
    loggedIn.update(_ => resp.ok);
}

const login = async (loginForm): Promise<void> => {
    const resp = await postData('/api/login', loginForm);
    loggedIn.update(_ => resp.ok);
}

const logout = async (): Promise<void> => {
    await postData('/api/logout');
    loggedIn.update(_ => false);
}