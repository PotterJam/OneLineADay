export { getData, postData }

const fetchMode = 'cors';
const credentialsOption = 'same-origin'; // for prod release might need this to include for a different sub-domain

const getCookie = (name: string) => {
  if (!document.cookie)
    return null;

  const cookiesWithName = document.cookie.split(';')
    .filter(c => c.trim().startsWith(name + '='));

  if (cookiesWithName.length === 0)
    return null;

  const cookie = cookiesWithName[0];
  return decodeURIComponent(cookie.substring(cookie.indexOf('=') + 1));
}

const getData = async (url = ''): Promise<Response> => {
    const response = await fetch(url, {
      method: 'GET',
      mode: fetchMode,
      cache: 'no-cache',
      credentials: credentialsOption,
      headers: {
        'Content-Type': 'application/json',
        'X-XSRF-TOKEN': getCookie('XSRF-TOKEN') || ""
      },
    });
    return response;
}

const postData = async (url = '', data = {}): Promise<Response> => {
    const response = await fetch(url, {
      method: 'POST',
      mode: fetchMode,
      cache: 'no-cache',
      credentials: credentialsOption,
      headers: {
        'Content-Type': 'application/json',
        'X-XSRF-TOKEN': getCookie('XSRF-TOKEN') || ""
      },
      body: JSON.stringify(data)
    });
    return response;
}