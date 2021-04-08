export { getData, postData }

const fetchMode = 'cors';
const credentialsOption = 'same-origin'; // for prod release might need this to include for a different sub-domain

const getData = async (url = ''): Promise<Response> => {
    const response = await fetch(url, {
      method: 'GET',
      mode: fetchMode,
      cache: 'no-cache',
      credentials: credentialsOption,
      headers: {
        'Content-Type': 'application/json'
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
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(data)
    });
    return response;
}