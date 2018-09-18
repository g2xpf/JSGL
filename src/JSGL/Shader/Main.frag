#version 330 core
precision mediump float;

uniform vec3 camera;
uniform mat4 invTransform;
in vec3 vNormal;
in vec4 vColor;
out vec4 color;

void main(){
    vec4 lay = normalize(vec4(1.0, 1.0, -1.0, 0.0));
    vec3 invCamera = normalize(invTransform * vec4(camera, 1.0)).xyz;
    vec3 invLight = normalize(invTransform * lay).xyz;
    vec3 half = normalize(invCamera + invLight);
    float ambient = pow(clamp(dot(vNormal, half), 0.0, 1.0), 50.0);
    float diffuse = clamp(dot(vNormal, invLight), 0.1, 1.0);

    vec4 env = vec4(vec3(0.1), 0.1);

    color = clamp(vColor * vec4(vec3(diffuse), 1.0) + env + vec4(vec3(ambient), 1.0), 0.0, 1.0);
}

