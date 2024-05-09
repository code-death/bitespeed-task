import axios from "axios";

export const API_URL = "https://finkraft-task-backend.onrender.com";

export default function callApi(endpoint, method = 'get', body) {
    let token = typeof window === 'undefined' ? '' : window.localStorage.getItem('token');
    let headers = {
        'content-type': 'application/json',
    };
    if (token && token !== '') {
        headers.Authorization = `Bearer ${token}`;
    }
    return axios({
        method: method,
        url: `${API_URL}/${endpoint}`,
        headers: headers,
        data: body ? JSON.stringify(body) : undefined,
    })
        .then((response) => {
            if (!response.ok) {
                return Promise.reject(response.data);
            }
            return response.data;
        })
        .catch((error) => error);
}

export function callUploadApi(endpoint, file) {
    const formData = new FormData();
    formData.append('transactions', file); // Assuming 'file' is the key for the file in the API

    return axios({
        method: 'post',
        url: `${API_URL}/${endpoint}`,
        headers: {
            'Content-Type': 'multipart/form-data', // Ensure correct content type for file upload
        },
        data: formData,
    })
        .then((response) => {
            return response.data; // No need to check for response.ok, axios handles HTTP status codes
        })
        .catch((error) => {
            return Promise.reject(error.response ? error.response.data : error.message);
        });
}
